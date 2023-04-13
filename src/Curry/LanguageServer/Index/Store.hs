{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Index.Store
    ( ModuleStoreEntry (..)
    , IndexStore (..)
    , storedModuleCount
    , storedSymbolCount
    , storedModule
    , storedModuleByIdent
    , storedModules
    , storedSymbols
    , storedSymbolsByIdent
    , storedSymbolsWithPrefix
    , storedSymbolsByQualIdent
    , storedModuleSymbolsWithPrefix
    , addWorkspaceDir
    , recompileModule
    , getModuleCount
    , getModule
    , getModuleList
    , getModuleAST
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Files.Filenames as CFN
import qualified Base.TopEnv as CT
import qualified CompilerEnv as CE

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch (..))
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
import Curry.LanguageServer.Utils.Logging (infoM, errorM, debugM, warnM)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Uri
import Data.Default
import Data.Function (on)
import Data.List (unionBy, isPrefixOf)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, maybeToList, mapMaybe, catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as TR
import qualified Language.LSP.Types as J
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), takeDirectory, takeExtension, takeFileName)
import qualified System.FilePath.Glob as G
import System.Process (readProcessWithExitCode)
import Language.LSP.Server (MonadLsp)

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
addWorkspaceDir :: (MonadState IndexStore m, MonadIO m, MonadLsp c m, MonadCatch m) => CFG.Config -> C.FileLoader -> FilePath -> m ()
addWorkspaceDir cfg fl dirPath = void $ runMaybeT $ do
    files <- lift $ findCurrySourcesInWorkspace cfg dirPath
    lift $ do
        mapM_ (\(i, (f, ip)) -> recompileFile i (length files) cfg fl ip (Just dirPath) f) (zip [1..] files)
        infoM $ "Added workspace directory " <> T.pack dirPath

-- | Recompiles the module entry with the given URI and stores the output.
recompileModule :: (MonadState IndexStore m, MonadIO m, MonadLsp c m, MonadCatch m) => CFG.Config -> C.FileLoader -> J.NormalizedUri -> m ()
recompileModule cfg fl uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    lift $ do
        recompileFile 1 1 cfg fl [] Nothing filePath
        debugM $ "Recompiled entry " <> T.pack (show uri)

-- | Finds the Curry source files along with its import paths in a workspace. Recognizes CPM projects.
findCurrySourcesInWorkspace :: (MonadIO m, MonadLsp c m) => CFG.Config -> FilePath -> m [(FilePath, [FilePath])]
findCurrySourcesInWorkspace cfg dirPath = do
    cpmProjPaths <- (takeDirectory <$>) <$> walkPackageJsons dirPath
    let projPaths = fromMaybe [dirPath] $ nothingIfNull cpmProjPaths
    nubOrdOn fst <$> join <$> mapM (findCurrySourcesInProject cfg) projPaths

-- | Finds the Curry source files in a (project) directory.
findCurrySourcesInProject :: (MonadIO m, MonadLsp c m) => CFG.Config -> FilePath -> m [(FilePath, [FilePath])]
findCurrySourcesInProject cfg dirPath = do
    let curryPath = CFG.cfgCurryPath cfg
        cpmPath = curryPath ++ " cypm"
        libPath binPath = takeDirectory (takeDirectory binPath) </> "lib"

    e <- liftIO $ doesFileExist $ dirPath </> "package.json"
    if e
        then do
            infoM $ "Found CPM project '" <> T.pack (takeFileName dirPath) <> "', searching for sources..."
            let projSrcFolder = dirPath </> "src"
            projSources <- walkCurrySourceFiles projSrcFolder

            infoM "Invoking CPM to fetch project configuration and dependencies..."
            result <- runCPMM $ do
                config <- invokeCPMConfig dirPath cpmPath
                deps   <- invokeCPMDeps   dirPath cpmPath
                return (config, deps)

            case result of
                Right (config, deps) -> do
                    let packagePath = fromJust $ lookup "PACKAGE_INSTALL_PATH" config
                        curryBinPath = fromJust $ lookup "CURRY_BIN" config
                        curryLibPath = libPath curryBinPath

                    infoM $ "Package path: " <> T.pack packagePath
                    infoM $ "Curry bin path: " <> T.pack curryBinPath
                    infoM $ "Curry lib path: " <> T.pack curryLibPath

                    let depPaths = (packagePath </>) . (</> "src") <$> deps
                        libPaths = [curryLibPath]

                    return $ map (, projSrcFolder : libPaths ++ depPaths) projSources
                Left err -> do
                    errorM $ "Could not fetch CPM configuration/dependencies: " <> T.pack err <> " (This might result in 'missing Prelude' errors!)"

                    return $ map (, []) projSources
        else do
            infoM $ "Found generic project '" <> T.pack (takeFileName dirPath) <> "', searching for sources..."

            (exitCode, fullCurryPath, _) <- liftIO $ readProcessWithExitCode "which" [curryPath] []
            let curryLibPath = libPath fullCurryPath
                libPaths | exitCode == ExitSuccess = [curryLibPath]
                         | otherwise               = []

            unless (exitCode == ExitSuccess) $
                warnM "Could not find default Curry libraries, this might result in 'missing Prelude' errors..."

            map (, libPaths) <$> walkCurrySourceFiles dirPath

-- | Recursively finds all CPM manifests in a directory.
walkPackageJsons :: (MonadIO m, MonadLsp c m) => FilePath -> m [FilePath]
walkPackageJsons = (filter ((== "package.json") . takeFileName) <$>) . walkFilesIgnoringHidden

-- | Recursively finds all Curry source files in a directory.
walkCurrySourceFiles :: (MonadIO m, MonadLsp c m) => FilePath -> m [FilePath]
walkCurrySourceFiles = (filter ((== ".curry") . takeExtension) <$>) . walkFilesIgnoringHidden

-- | Recursively finds Curry source files, ignoring directories starting with dots
--   and those specified in .curry-language-server-ignore.
--   TODO: Respect parent gitignore also in subdirectories (may require changes to walkFilesWith
--         to aggregate the state across recursive calls, perhaps by requiring a Monoid instance?)
walkFilesIgnoringHidden :: (MonadIO m, MonadLsp c m) => FilePath -> m [FilePath]
walkFilesIgnoringHidden = walkFilesWith $ WalkConfiguration
    { wcOnEnter      = \fp -> do
        ignorePaths <- filterM (liftIO . doesFileExist) $ (fp </>) <$> [".curry-language-server-ignore", ".gitignore"]
        ignored     <- join <$> mapM readIgnoreFile ignorePaths
        unless (null ignored) $
            infoM $ "In '" <> T.pack (takeFileName fp) <> "' ignoring " <> T.pack (show (G.decompile <$> ignored))
        return $ Just ignored
    , wcShouldIgnore = \ignored fp -> do
        isDir <- liftIO $ doesDirectoryExist fp
        let fn              = takeFileName fp
            matchesFn pat   = any (G.match pat) $ catMaybes [Just fn, if isDir then Just (fn ++ "/") else Nothing]
            matchingIgnores = filter matchesFn ignored
        unless (null matchingIgnores) $
            debugM $ "Ignoring '" <> T.pack fn <> "' since it matches " <> T.pack (show (G.decompile <$> matchingIgnores))
        return $ not (null matchingIgnores) || "." `isPrefixOf` fn
    }

-- | Reads the given ignore file, fetching the ignored (relative) paths.
readIgnoreFile :: MonadIO m => FilePath -> m [G.Pattern]
readIgnoreFile = liftIO . (map (G.simplify . G.compile . T.unpack) . filter useLine . T.lines <$>) . TIO.readFile
    where useLine l = not (T.null l) && not ("#" `T.isPrefixOf` l)

-- | Recompiles the entry with its dependencies using explicit paths and stores the output.
recompileFile :: (MonadState IndexStore m, MonadIO m, MonadLsp c m, MonadCatch m) => Int -> Int -> CFG.Config -> C.FileLoader -> [FilePath] -> Maybe FilePath -> FilePath -> m ()
recompileFile i total cfg fl importPaths dirPath filePath = void $ do
    infoM $ "[" <> T.pack (show i) <> " of " <> T.pack (show total) <> "] (Re)compiling file " <> T.pack (takeFileName filePath)

    uri <- filePathToNormalizedUri filePath
    ms <- gets idxModules

    let defEntry = def { mseWorkspaceDir = dirPath, mseImportPaths = importPaths }
        outDirPath = CFN.defaultOutDir </> "language-server"
        importPaths' = outDirPath : mseImportPaths (M.findWithDefault defEntry uri ms)
        aux = C.CompileAuxiliary { C.fileLoader = fl }

    (co, cs) <- catch
        (C.compileCurryFileWithDeps cfg aux importPaths' outDirPath filePath)
        (\e -> return $ C.failedCompilation $ "Compilation failed: " ++ show (e :: SomeException))

    let msgNormUri msg = (fromMaybe uri <$>) $ runMaybeT $ do
            uri' <- currySpanInfo2Uri $ CM.msgSpanInfo msg
            normalizeUriWithPath uri'

    -- Ignore parses from interface files, only consider source files for now
    asts <- mapM (\(fp, mdl) -> (, mdl) <$> filePathToNormalizedUri fp) $ filter ((".curry" `T.isSuffixOf`) . T.pack . fst) co

    warns  <- groupIntoMapByM msgNormUri $ C.csWarnings cs
    errors <- groupIntoMapByM msgNormUri $ C.csErrors cs

    debugM $ "Recompiled module paths: " <> T.pack (show (fst <$> asts))

    -- Update store with compiled modules

    let modifyEntry f = M.alter (Just . f . fromMaybe defEntry)

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
        valueSymbols <- join <$> mapM toSymbols (CT.allBindings $ CE.valueEnv env)
        typeSymbols  <- join <$> mapM toSymbols (CT.allBindings $ CE.tyConsEnv env)
        modSymbols   <- join <$> mapM toSymbols (nubOrdOn ppToText $ mapMaybe (CI.qidModule . fst) $ CT.allImports (CE.valueEnv env))

        let symbolDelta = valueSymbols ++ typeSymbols ++ modSymbols
            combiner = unionBy ((==) `on` (\s' -> (sKind s', sQualIdent s', sIsFromCurrySource s')))
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
