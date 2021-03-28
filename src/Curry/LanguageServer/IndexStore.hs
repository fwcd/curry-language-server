{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Curry.LanguageServer.IndexStore (
    ModuleStoreEntry (..),
    SymbolStoreEntry (..),
    IndexStore (..),
    emptyStore,
    storedModuleCount,
    storedSymbolCount,
    storedModule,
    storedModuleByIdent,
    storedModules,
    storedSymbols,
    storedSymbolsWithPrefix,
    storedSymbolsByQualIdent,
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
import Control.Lens ((^.))
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.CPM.Config (invokeCPMConfig)
import Curry.LanguageServer.CPM.Deps (invokeCPMDeps)
import Curry.LanguageServer.CPM.Monad (runCPMM)
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import Curry.LanguageServer.Utils.Uri
import Data.Default
import Data.Function (on)
import Data.List (nubBy, unionBy)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe, fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as TR
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), takeDirectory, takeExtension, takeFileName)
import System.Log.Logger
import System.Process (readProcessWithExitCode)

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data ModuleStoreEntry = ModuleStoreEntry { mseModuleAST :: Maybe ModuleAST
                                         , mseCompilerEnv :: Maybe CE.CompilerEnv
                                         , mseErrorMessages :: [CM.Message]
                                         , mseWarningMessages :: [CM.Message]
                                         , mseWorkspaceDir :: Maybe FilePath
                                         , mseImportPaths :: [FilePath]
                                         }

-- | An index store entry containing a symbol.
data SymbolStoreEntry = SymbolStoreEntry { sseSymbol :: J.SymbolInformation
                                         , sseQualIdent :: CI.QualIdent
                                         }

-- | An in-memory map of URIs to parsed modules and
-- unqualified symbol names to actual symbols/symbol information.
-- Since (unqualified) symbol names can be ambiguous, a trie leaf
-- holds a list of symbol entries rather than just a single one.
data IndexStore = IndexStore { idxModules :: M.Map J.NormalizedUri ModuleStoreEntry
                             , idxSymbols :: TR.Trie [SymbolStoreEntry]
                             }

instance Default ModuleStoreEntry where
    def = ModuleStoreEntry { mseModuleAST = Nothing
                           , mseCompilerEnv = Nothing
                           , mseWarningMessages = []
                           , mseErrorMessages = []
                           , mseWorkspaceDir = Nothing
                           , mseImportPaths = []
                           }

-- | Fetches an empty index store.
emptyStore :: IndexStore
emptyStore = IndexStore { idxModules = M.empty, idxSymbols = TR.empty }

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

-- | Fetches the given (unqualified) symbol names in the store.
storedSymbols :: T.Text -> IndexStore -> [SymbolStoreEntry]
storedSymbols t = join . maybeToList . TR.lookup (TE.encodeUtf8 t) . idxSymbols

-- | Fetches the list of symbols starting with the given prefix.
storedSymbolsWithPrefix :: T.Text -> IndexStore -> [SymbolStoreEntry]
storedSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . idxSymbols

-- | Fetches stored symbols by qualified identifier.
storedSymbolsByQualIdent :: CI.QualIdent -> IndexStore -> [SymbolStoreEntry]
storedSymbolsByQualIdent q = filter (qidEq q . sseQualIdent) . storedSymbols name
    where name = T.pack $ CI.idName $ CI.qidIdent q
          qidEq = (==) `on` ppToText

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
    nubBy ((==) `on` fst) <$> join <$> mapM (findCurrySourcesInProject cfg) projPaths

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

    ms <- gets idxModules
    ss <- gets idxSymbols
    uri <- liftIO $ filePathToNormalizedUri filePath

    let previous :: J.NormalizedUri -> ModuleStoreEntry
        previous = flip (M.findWithDefault $ def { mseWorkspaceDir = dirPath, mseImportPaths = importPaths }) ms
        outDirPath = CFN.defaultOutDir </> "language-server"
        importPaths' = outDirPath : mseImportPaths (previous uri)

    result <- liftIO $ catch
        (C.compileCurryFileWithDeps cfg fl importPaths' outDirPath filePath)
        (\e -> return $ C.failedCompilation $ "Compilation failed: " ++ show (e :: SomeException))
    
    case result of
        Left errs -> modify $ \s -> s { idxModules = M.insert uri ((previous uri) { mseErrorMessages = errs, mseWarningMessages = [] }) ms }
        Right (o, warns) -> do liftIO $ debugM "cls.indexStore" $ "Recompiled module paths: " ++ show (fst <$> asts)
                               ws <- liftIO $ groupIntoMapByM msgNormUri warns
                               moduleDelta <- liftIO
                                            $ sequence
                                            $ (\(fp, ast) -> do u <- filePathToNormalizedUri fp
                                                                return (u, (previous u) { mseModuleAST = Just ast
                                                                                        , mseCompilerEnv = Just env
                                                                                        , mseErrorMessages = []
                                                                                        , mseWarningMessages = M.findWithDefault [] (Just u) ws
                                                                                        }))
                                            <$> asts

                               valueSymbols <- liftIO $ join <$> mapM bindingToQualSymbols (CT.allBindings $ CE.valueEnv env)
                               typeSymbols  <- liftIO $ join <$> mapM bindingToQualSymbols (CT.allBindings $ CE.tyConsEnv env)

                               let symbolDelta = (\(qid, s) -> (TE.encodeUtf8 $ s ^. J.name, [SymbolStoreEntry s qid])) <$> (valueSymbols ++ typeSymbols)
                               liftIO $ debugM "cls.indexStore" $ "Inserting " ++ show (length symbolDelta) ++ " symbol(s)"

                               modify $ \s -> s { idxModules = insertAll moduleDelta ms
                                                , idxSymbols = insertAllIntoTrieWith (unionBy ((==) `on` sseQualIdent)) symbolDelta ss
                                                }
            where env = C.mseCompilerEnv o
                  asts = C.mseModuleASTs o
                  msgNormUri msg = runMaybeT $ do
                      uri' <- currySpanInfo2Uri $ CM.msgSpanInfo msg
                      liftIO $ normalizeUriWithPath uri'

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
