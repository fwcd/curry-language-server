{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Index.Store (
    ModuleStoreEntry (..),
    IndexStore (..),
    storedModuleCount,
    storedSymbolCount,
    storedModule,
    storedModuleByIdent,
    storedModules,
    storedSymbols,
    storedSymbolsByIdent,
    storedSymbolsWithPrefix,
    storedSymbolsByQualIdent,
    storedModuleSymbolsWithPrefix,
    addWorkspaceDir,
    recompileModule,
    getModuleCount,
    getModule,
    getModuleList,
    getModuleAST
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Files.Filenames as CFN
import qualified Base.TopEnv as CT
import qualified CompilerEnv as CE

import Control.Exception (catch, SomeException)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.CPM.Config (invokeCPMConfig)
import Curry.LanguageServer.CPM.Deps (invokeCPMDeps)
import Curry.LanguageServer.CPM.Monad (runCPMM)
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Index.Convert
import Curry.LanguageServer.Index.Symbol
import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import Curry.LanguageServer.Utils.Uri
import Data.Default
import Data.Function (on)
import Data.List (unionBy)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe, fromMaybe, maybeToList, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as TR
import qualified Language.LSP.Types as J
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), takeDirectory, takeExtension, takeFileName)
import System.Log.Logger
import System.Process (readProcessWithExitCode)

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data ModuleStoreEntry = ModuleStoreEntry { mseModuleAST :: Maybe ModuleAST
                                         , mseErrorMessages :: [CM.Message]
                                         , mseWarningMessages :: [CM.Message]
                                         , mseWorkspaceDir :: Maybe FilePath
                                         , mseImportPaths :: [FilePath]
                                         }



-- | An in-memory map of URIs to parsed modules and
-- unqualified symbol names to actual symbols/symbol information.
-- Since (unqualified) symbol names can be ambiguous, a trie leaf
-- holds a list of symbol entries rather than just a single one.
data IndexStore = IndexStore { idxModules :: M.Map J.NormalizedUri ModuleStoreEntry
                               -- Symbols keyed by unqualified name
                             , idxSymbols :: TR.Trie [Symbol]
                               -- Module symbols keyed by qualified name
                             , idxModuleSymbols :: TR.Trie [Symbol]
                             }

instance Default ModuleStoreEntry where
    def = ModuleStoreEntry { mseModuleAST = Nothing
                           , mseWarningMessages = []
                           , mseErrorMessages = []
                           , mseWorkspaceDir = Nothing
                           , mseImportPaths = []
                           }

instance Default IndexStore where
    def = IndexStore { idxModules = M.empty, idxSymbols = TR.empty, idxModuleSymbols = TR.empty }

-- | Fetches the number of stored modules.
storedModuleCount :: IndexStore -> Int
storedModuleCount = M.size . idxModules

-- | Fetches the number of stored symbols.
storedSymbolCount :: IndexStore -> Int
storedSymbolCount = TR.size . idxSymbols

-- | Fetches the given entry in the store.
storedModule :: J.NormalizedUri -> IndexStore -> Maybe ModuleStoreEntry
storedModule uri = M.lookup uri . idxModules

-- | Fetches an entry in the store by module identifier.
storedModuleByIdent :: CI.ModuleIdent -> IndexStore -> IO (Maybe ModuleStoreEntry)
storedModuleByIdent mident store = flip storedModule store <$> uri
    where filePath = CFN.moduleNameToFile mident <.> "curry"
          uri = filePathToNormalizedUri filePath

-- | Fetches the entries in the store as a list.
storedModules :: IndexStore -> [(J.NormalizedUri, ModuleStoreEntry)]
storedModules = M.toList . idxModules

-- | Fetches all symbols.
storedSymbols :: IndexStore -> [Symbol]
storedSymbols = join . TR.toListBy (const id) . idxSymbols

-- | Fetches the given (unqualified) symbol names in the store.
storedSymbolsByIdent :: T.Text -> IndexStore -> [Symbol]
storedSymbolsByIdent t = join . maybeToList . TR.lookup (TE.encodeUtf8 t) . idxSymbols

-- | Fetches the list of symbols starting with the given prefix.
storedSymbolsWithPrefix :: T.Text -> IndexStore -> [Symbol]
storedSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . idxSymbols

-- | Fetches stored symbols by qualified identifier.
storedSymbolsByQualIdent :: CI.QualIdent -> IndexStore -> [Symbol]
storedSymbolsByQualIdent q = filter ((== ppToText q) . sQualIdent) . storedSymbolsByIdent name
    where name = T.pack $ CI.idName $ CI.qidIdent q

-- | Fetches stored module symbols starting with the given prefix.
storedModuleSymbolsWithPrefix :: T.Text -> IndexStore -> [Symbol]
storedModuleSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . idxModuleSymbols

-- | Compiles the given directory recursively and stores its entries.
addWorkspaceDir :: (MonadState IndexStore m, MonadIO m) => CFG.Config -> C.FileLoader -> FilePath -> m ()
addWorkspaceDir cfg fl dirPath = void $ runMaybeT $ do
    files <- liftIO $ findCurrySourcesInWorkspace cfg dirPath
    sequence_ $ (\(i, (f, ip)) -> recompileFile i (length files) cfg fl ip (Just dirPath) f) <$> zip [1..] files
    liftIO $ infoM "cls.indexStore" $ "Added workspace directory " ++ dirPath

-- | Recompiles the module entry with the given URI and stores the output.
recompileModule :: (MonadState IndexStore m, MonadIO m) => CFG.Config -> C.FileLoader -> J.NormalizedUri -> m ()
recompileModule cfg fl uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    recompileFile 1 1 cfg fl [] Nothing filePath
    liftIO $ debugM "cls.indexStore" $ "Recompiled entry " ++ show uri

-- | Finds the Curry source files along with its import paths in a workspace. Recognizes CPM projects.
findCurrySourcesInWorkspace :: CFG.Config -> FilePath -> IO [(FilePath, [FilePath])]
findCurrySourcesInWorkspace cfg dirPath = do
    cpmProjPaths <- (takeDirectory <$>) <$> walkPackageJsons dirPath
    let projPaths = fromMaybe [dirPath] $ nothingIfNull cpmProjPaths
    nubOrdOn fst <$> join <$> mapM (findCurrySourcesInProject cfg) projPaths

-- | Finds the Curry source files in a (project) directory.
findCurrySourcesInProject :: CFG.Config -> FilePath -> IO [(FilePath, [FilePath])]
findCurrySourcesInProject cfg dirPath = do
    let curryPath = CFG.cfgCurryPath cfg
        cpmPath = curryPath ++ " cypm"
        libPath binPath = takeDirectory (takeDirectory binPath) </> "lib"

    e <- doesFileExist $ dirPath </> "package.json"
    if e
        then do
            liftIO $ infoM "cls.indexStore" $ "Found CPM project '" <> takeFileName dirPath <> "', searching for sources..."
            let projSrcFolder = dirPath </> "src"
            projSources <- walkCurrySourceFiles projSrcFolder

            liftIO $ infoM "cls.indexStore" "Invoking CPM to fetch project configuration and dependencies..."
            result <- runCPMM $ do
                config <- invokeCPMConfig dirPath cpmPath
                deps   <- invokeCPMDeps   dirPath cpmPath
                return (config, deps)
            
            case result of
                Right (config, deps) -> do
                    let packagePath = fromJust $ lookup "PACKAGE_INSTALL_PATH" config
                        curryBinPath = fromJust $ lookup "CURRY_BIN" config
                        curryLibPath = libPath curryBinPath
                    
                    liftIO $ infoM "cls.indexStore" $ "Package path: " ++ packagePath
                    liftIO $ infoM "cls.indexStore" $ "Curry bin path: " ++ curryBinPath
                    liftIO $ infoM "cls.indexStore" $ "Curry lib path: " ++ curryLibPath

                    let depPaths = (packagePath </>) . (</> "src") <$> deps
                        libPaths = [curryLibPath]

                    return $ map (, projSrcFolder : libPaths ++ depPaths) projSources
                Left err -> do
                    liftIO $ errorM "cls.indexStore" $ "Could not fetch CPM configuration/dependencies: " ++ err ++ " (This might result in 'missing Prelude' errors!)"

                    return $ map (, []) projSources
        else do
            liftIO $ infoM "cls.indexStore" $ "Found generic project '" <> takeFileName dirPath <> "', searching for sources..."

            (exitCode, fullCurryPath, _) <- readProcessWithExitCode "which" [curryPath] []
            let curryLibPath = libPath fullCurryPath
                libPaths | exitCode == ExitSuccess = [curryLibPath]
                         | otherwise               = []
            
            unless (exitCode == ExitSuccess) $
                warningM "cls.indexStore" "Could not find default Curry libraries, this might result in 'missing Prelude' errors..."

            map (, libPaths) <$> walkCurrySourceFiles dirPath

-- | Recursively finds all CPM manifests in a directory.
walkPackageJsons :: FilePath -> IO [FilePath]
walkPackageJsons = (filter ((== "package.json") . takeFileName) <$>) . walkFilesIgnoringHidden

-- | Recursively finds all Curry source files in a directory.
walkCurrySourceFiles :: FilePath -> IO [FilePath]
walkCurrySourceFiles = (filter ((== ".curry") . takeExtension) <$>) . walkFilesIgnoringHidden

-- | Recursively finds Curry source files, ignoring directories starting with dots.
walkFilesIgnoringHidden :: FilePath -> IO [FilePath]
walkFilesIgnoringHidden = walkFilesIgnoring ((== Just '.') . listToMaybe . takeFileName)

-- | Recompiles the entry with its dependencies using explicit paths and stores the output.
recompileFile :: (MonadState IndexStore m, MonadIO m) => Int -> Int -> CFG.Config -> C.FileLoader -> [FilePath] -> Maybe FilePath -> FilePath -> m ()
recompileFile i total cfg fl importPaths dirPath filePath = void $ do
    liftIO $ infoM "cls.indexStore" $ "[" ++ show i ++ " of " ++ show total ++ "] (Re)compiling file " ++ takeFileName filePath

    uri <- liftIO $ filePathToNormalizedUri filePath
    ms <- gets idxModules

    let defEntry = def { mseWorkspaceDir = dirPath, mseImportPaths = importPaths }
        outDirPath = CFN.defaultOutDir </> "language-server"
        importPaths' = outDirPath : mseImportPaths (M.findWithDefault defEntry uri ms)

    (co, cs) <- liftIO $ catch
        (C.compileCurryFileWithDeps cfg fl importPaths' outDirPath filePath)
        (\e -> return $ C.failedCompilation $ "Compilation failed: " ++ show (e :: SomeException))
    
    let msgNormUri msg = (fromMaybe uri <$>) $ runMaybeT $ do
            uri' <- currySpanInfo2Uri $ CM.msgSpanInfo msg
            liftIO $ normalizeUriWithPath uri'

    -- Ignore parses from interface files, only consider source files for now
    asts <- liftIO $ mapM (\(fp, mdl) -> (, mdl) <$> filePathToNormalizedUri fp) $ filter ((".curry" `T.isSuffixOf`) . T.pack . fst) co

    warns  <- liftIO $ groupIntoMapByM msgNormUri $ C.csWarnings cs
    errors <- liftIO $ groupIntoMapByM msgNormUri $ C.csErrors cs

    liftIO $ debugM "cls.indexStore" $ "Recompiled module paths: " ++ show (fst <$> asts)

    -- Update store with compiled modules

    let modifyEntry f uri' ms' = M.alter (Just . f . fromMaybe defEntry) uri' ms'

    forM_ asts $ \(uri', (env, ast)) -> do
        -- Update module store
        let updateEntry e = e
                { mseWarningMessages = M.findWithDefault [] uri' warns
                , mseErrorMessages = M.findWithDefault [] uri' errors
                , mseModuleAST = Just ast
                -- , mseCompilerEnv = Just env
                }
        modify $ \s -> s { idxModules = modifyEntry updateEntry uri' $ idxModules s }

        -- Update symbol store
        valueSymbols <- liftIO $ join <$> mapM toSymbols (CT.allBindings $ CE.valueEnv env)
        typeSymbols  <- liftIO $ join <$> mapM toSymbols (CT.allBindings $ CE.tyConsEnv env)
        modSymbols   <- liftIO $ join <$> mapM toSymbols (nubOrdOn ppToText $ mapMaybe (CI.qidModule . fst) $ CT.allImports (CE.valueEnv env))

        let symbolDelta = valueSymbols ++ typeSymbols ++ modSymbols
        liftIO $ debugM "cls.indexStore" $ "Inserting " ++ show (length symbolDelta) ++ " symbol(s)"

        let combiner = unionBy ((==) `on` (\s' -> (sQualIdent s', sIsFromCurrySource s')))
        modify $ \s -> s
            { idxSymbols = insertAllIntoTrieWith combiner ((\s' -> (TE.encodeUtf8 $ sIdent s', [s'])) <$> symbolDelta) $ idxSymbols s
            , idxModuleSymbols = insertAllIntoTrieWith (unionBy ((==) `on` sQualIdent)) ((\s' -> (TE.encodeUtf8 $ sQualIdent s', [s'])) <$> modSymbols) $ idxModuleSymbols s
            }
    
    -- Update store with messages from files that were not successfully compiled

    let uris = S.fromList $ fst <$> asts
        other = filter ((`S.notMember` uris) . fst) . M.toList
    
    forM_ (other warns) $ \(uri', msgs) -> do
        let updateEntry e = e { mseWarningMessages = msgs }
        modify $ \s -> s { idxModules = modifyEntry updateEntry uri' $ idxModules s }

    forM_ (other errors) $ \(uri', msgs) -> do
        let updateEntry e = e { mseErrorMessages = msgs }
        modify $ \s -> s { idxModules = modifyEntry updateEntry uri' $ idxModules s }

-- | Fetches the number of module entries in the store in a monadic way.
getModuleCount :: (MonadState IndexStore m) => m Int
getModuleCount = gets storedModuleCount

-- | Fetches a module entry in the store in a monadic way.
getModule :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleStoreEntry
getModule uri = liftMaybe =<< gets (storedModule uri)

-- | Fetches the module entries in the store as a list in a monadic way.
getModuleList :: (MonadState IndexStore m) => m [(J.NormalizedUri, ModuleStoreEntry)]
getModuleList = gets storedModules

-- | Fetches the AST for a given URI in the store in a monadic way.
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleAST
getModuleAST uri = (liftMaybe . mseModuleAST) =<< getModule uri
