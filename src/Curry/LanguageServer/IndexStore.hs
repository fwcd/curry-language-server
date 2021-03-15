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
    storedSymbolsWithPrefix,
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

import Control.Applicative (Alternative (..))
import Control.Exception (catch, SomeException)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.CPM.Config (invokeCPMConfig)
import Curry.LanguageServer.CPM.Deps (invokeCPMDeps)
import Curry.LanguageServer.CPM.Monad (runCM)
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import Curry.LanguageServer.Utils.Uri
import Data.Default
import Data.Function (on)
import Data.List (nubBy, unionBy)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as TR
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Directory (doesFileExist)
import System.FilePath
import System.Log.Logger

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data ModuleStoreEntry = ModuleStoreEntry { moduleAST :: Maybe ModuleAST
                                         , compilerEnv :: Maybe CE.CompilerEnv
                                         , errorMessages :: [CM.Message]
                                         , warningMessages :: [CM.Message]
                                         , workspaceDir :: Maybe FilePath
                                         , importPaths :: [FilePath]
                                         }

-- | An index store entry containing a symbol.
data SymbolStoreEntry = SymbolStoreEntry { symbol :: J.SymbolInformation
                                         , qualIdent :: CI.QualIdent
                                         }

-- | An in-memory map of URIs to parsed modules and
-- unqualified symbol names to actual symbols/symbol information.
-- Since (unqualified) symbol names can be ambiguous, a trie leaf
-- holds a list of symbol entries rather than just a single one.
data IndexStore = IndexStore { modules :: M.Map J.NormalizedUri ModuleStoreEntry
                             , symbols :: TR.Trie [SymbolStoreEntry]
                             }

instance Default ModuleStoreEntry where
    def = ModuleStoreEntry { moduleAST = Nothing
                           , compilerEnv = Nothing
                           , warningMessages = []
                           , errorMessages = []
                           , workspaceDir = Nothing
                           , importPaths = []
                           }

-- | Fetches an empty index store.
emptyStore :: IndexStore
emptyStore = IndexStore { modules = M.empty, symbols = TR.empty }

-- | Fetches the number of stored modules.
storedModuleCount :: IndexStore -> Int
storedModuleCount = M.size . modules

-- | Fetches the number of stored symbols.
storedSymbolCount :: IndexStore -> Int
storedSymbolCount = TR.size . symbols

-- | Fetches the given entry in the store.
storedModule :: J.NormalizedUri -> IndexStore -> Maybe ModuleStoreEntry
storedModule uri = M.lookup uri . modules

-- | Fetches an entry in the store by module identifier.
storedModuleByIdent :: CI.ModuleIdent -> IndexStore -> IO (Maybe ModuleStoreEntry)
storedModuleByIdent mident store = flip storedModule store <$> uri
    where filePath = CFN.moduleNameToFile mident <.> "curry"
          uri = filePathToNormalizedUri filePath

-- | Fetches the entries in the store as a list.
storedModules :: IndexStore -> [(J.NormalizedUri, ModuleStoreEntry)]
storedModules = M.toList . modules

-- | Fetches the list of symbols starting with the given prefix.
storedSymbolsWithPrefix :: T.Text -> IndexStore -> [SymbolStoreEntry]
storedSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . symbols

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
    e <- doesFileExist $ dirPath </> "package.json"
    if e
        then do
            liftIO $ infoM "cls.indexStore" $ "Found Curry Package Manager project '" <> takeFileName dirPath <> "', searching for sources..."
            projSources <- walkCurrySourceFiles $ dirPath </> "src"

            liftIO $ infoM "cls.indexStore" "Invoking CPM to fetch project configuration and dependencies..."
            result <- runCM $ do
                config <- invokeCPMConfig dirPath $ CFG.cpmPath cfg
                deps   <- invokeCPMDeps   dirPath $ CFG.cpmPath cfg
                return (config, deps)
            
            case result of
                Right (config, deps) -> do
                    let packagePath = fromJust $ lookup "PACKAGE_INSTALL_PATH" config
                        curryBinPath = fromJust $ lookup "CURRY_BIN" config
                        curryLibPath = takeDirectory (takeDirectory curryBinPath) </> "lib"
                    
                    liftIO $ infoM "cls.indexStore" $ "Package path: " ++ packagePath
                    liftIO $ infoM "cls.indexStore" $ "Curry bin path: " ++ curryBinPath
                    liftIO $ infoM "cls.indexStore" $ "Curry lib path: " ++ curryLibPath

                    depSources <- join <$> mapM walkCurrySourceFiles ((packagePath </>) <$> deps)
                    libSources <- walkCurrySourceFiles curryLibPath

                    return $ map (, libSources ++ depSources) projSources
                Left err -> do
                    liftIO $ errorM "cls.indexStore" $ "Could not fetch CPM configuration/dependencies: " ++ err

                    return $ map (, []) projSources
        else map (, []) <$> walkCurrySourceFiles dirPath

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

    ms <- gets modules
    ss <- gets symbols
    uri <- liftIO $ filePathToNormalizedUri filePath

    let previous :: J.NormalizedUri -> ModuleStoreEntry
        previous = flip (M.findWithDefault $ def { workspaceDir = dirPath, importPaths = importPaths  }) ms
        outDirPath = CFN.defaultOutDir </> "language-server"
        -- TODO: Apply previous import paths
        importPaths' = outDirPath : importPaths
    result <- liftIO $ catch (C.compileCurryFileWithDeps cfg fl importPaths' outDirPath filePath) (\e -> return $ C.failedCompilation $ "Compilation failed: " ++ show (e :: SomeException))
    
    case result of
        Left errs -> modify $ \s -> s { modules = M.insert uri ((previous uri) { errorMessages = errs, warningMessages = [] }) ms }
        Right (o, warns) -> do liftIO $ debugM "cls.indexStore" $ "Recompiled module paths: " ++ show (fst <$> asts)
                               ws <- liftIO $ groupIntoMapByM msgNormUri warns
                               moduleDelta <- liftIO
                                            $ sequence
                                            $ (\(fp, ast) -> do u <- filePathToNormalizedUri fp
                                                                return (u, (previous u) { moduleAST = Just ast,
                                                                                          compilerEnv = Just env,
                                                                                          errorMessages = [],
                                                                                          warningMessages = M.findWithDefault [] (Just u) ws }))
                                            <$> asts

                               valueSymbols <- liftIO $ join <$> mapM bindingToQualSymbols (CT.allBindings $ CE.valueEnv env)
                               typeSymbols  <- liftIO $ join <$> mapM bindingToQualSymbols (CT.allBindings $ CE.tyConsEnv env)

                               let symbolDelta = (\(qid, s) -> (TE.encodeUtf8 $ s ^. J.name, [SymbolStoreEntry s qid])) <$> (valueSymbols ++ typeSymbols)
                               liftIO $ debugM "cls.indexStore" $ "Inserting " ++ show (length symbolDelta) ++ " symbol(s)"

                               modify $ \s -> s { modules = insertAll moduleDelta ms,
                                                  symbols = insertAllIntoTrieWith (unionBy $ \x y -> qualIdent x == qualIdent y) symbolDelta ss }
            where env = C.compilerEnv o
                  asts = C.moduleASTs o
                  msgNormUri msg = runMaybeT $ do
                      uri <- currySpanInfo2Uri $ CM.msgSpanInfo msg
                      liftIO $ normalizeUriWithPath uri

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
getModuleAST uri = (liftMaybe . moduleAST) =<< getModule uri
