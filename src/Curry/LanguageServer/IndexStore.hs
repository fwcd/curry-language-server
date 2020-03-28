{-# LANGUAGE FlexibleContexts #-}
module Curry.LanguageServer.IndexStore (
    ModuleStoreEntry (..),
    IndexStore (..),
    emptyStore,
    storedModuleCount,
    storedModule,
    storedModuleByIdent,
    storedModules,
    addWorkspaceDir,
    recompileEntry,
    getCount,
    getEntry,
    getEntries,
    getModuleAST
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Files.Filenames as CFN
import qualified Curry.Syntax as CS
import qualified CompilerEnv as CE

import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.Config
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import Curry.LanguageServer.Utils.Uri
import qualified Data.ByteString as B
import qualified Data.Trie as TR
import Data.Default
import Data.List (delete)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Language.Haskell.LSP.Types as J
import System.FilePath

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data ModuleStoreEntry = ModuleStoreEntry { moduleAST :: Maybe ModuleAST,
                                           compilerEnv :: Maybe CE.CompilerEnv,
                                           errorMessages :: [CM.Message],
                                           warningMessages :: [CM.Message],
                                           workspaceDir :: Maybe FilePath }

-- | An in-memory map of URIs to parsed modules and
-- fully-qualified symbol names to actual symbols/symbol information.
data IndexStore = IndexStore { modules :: M.Map J.NormalizedUri ModuleStoreEntry,
                               symbols :: TR.Trie J.DocumentSymbol }

instance Default ModuleStoreEntry where
    def = ModuleStoreEntry { moduleAST = Nothing, compilerEnv = Nothing, warningMessages = [], errorMessages = [], workspaceDir = Nothing }

-- | Fetches an empty index store.
emptyStore :: IndexStore
emptyStore = IndexStore { modules = M.empty, symbols = TR.empty }

-- | Fetches the number of stored modules.
storedModuleCount :: IndexStore -> Int
storedModuleCount = M.size . modules

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

-- | Compiles the given directory recursively and stores its entries.
addWorkspaceDir :: (MonadState IndexStore m, MonadIO m) => Config -> FilePath -> m ()
addWorkspaceDir cfg dirPath = void $ runMaybeT $ do
    files <- liftIO $ walkCurrySourceFiles dirPath
    sequence $ recompileFile cfg (Just dirPath) <$> files
    liftIO $ logs INFO $ "indexStore: Added workspace directory " ++ dirPath

-- | Recompiles the entry with the given URI and stores the output.
recompileEntry :: (MonadState IndexStore m, MonadIO m) => Config -> J.NormalizedUri -> m ()
recompileEntry cfg uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    recompileFile cfg Nothing filePath
    liftIO $ logs INFO $ "indexStore: Recompiled entry " ++ show uri

-- | Finds all Curry source files in a directory.
walkCurrySourceFiles :: FilePath -> IO [FilePath]
walkCurrySourceFiles = (filter ((== ".curry") . takeExtension) <$>) . walkFiles

-- | Recompiles the entry with its dependencies using explicit paths and stores the output.
recompileFile :: (MonadState IndexStore m, MonadIO m) => Config -> Maybe FilePath -> FilePath -> m ()
recompileFile cfg dirPath filePath = void $ do
    liftIO $ logs INFO $ "indexStore: (Re-)compiling file " ++ takeFileName filePath
    let outDirPath = CFN.currySubdir </> ".language-server"
        importPaths = [outDirPath]
    result <- liftIO $ C.compileCurryFileWithDeps cfg importPaths outDirPath filePath
    
    m <- modules <$> get
    uri <- liftIO $ filePathToNormalizedUri filePath
    let previous :: J.NormalizedUri -> ModuleStoreEntry
        previous = flip (M.findWithDefault $ def { workspaceDir = dirPath }) m
    case result of
        Left errs -> modify $ \s -> s { modules = M.insert uri ((previous uri) { errorMessages = errs, warningMessages = [] }) m }
        Right (o, warns) -> do liftIO $ logs INFO $ "indexStore: Recompiled module paths: " ++ show (fst <$> asts)
                               ws <- liftIO $ groupIntoMapByM msgNormUri warns
                               delta <- liftIO
                                            $ sequence
                                            $ (\(fp, ast) -> do u <- filePathToNormalizedUri fp
                                                                return (u, (previous u) { moduleAST = Just ast,
                                                                                          compilerEnv = Just env,
                                                                                          errorMessages = [],
                                                                                          warningMessages = M.findWithDefault [] (Just u) ws }))
                                            <$> asts
                               modify $ \s -> s { modules = insertAll delta m }
            where env = C.compilerEnv o
                  asts = C.moduleASTs o
                  msgNormUri msg = runMaybeT $ do
                      uri <- curryPos2Uri =<< (liftMaybe $ CM.msgPos msg)
                      liftIO $ normalizeUriWithPath uri

-- | Fetches the number of entries in the store in a monadic way.
getCount :: (MonadState IndexStore m) => m Int
getCount = storedModuleCount <$> get

-- | Fetches an entry in the store in a monadic way.
getEntry :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleStoreEntry
getEntry uri = liftMaybe =<< storedModule uri <$> get

-- | Fetches the entries in the store as a list in a monadic way.
getEntries :: (MonadState IndexStore m) => m [(J.NormalizedUri, ModuleStoreEntry)]
getEntries = storedModules <$> get

-- | Fetches the AST for a given URI in the store in a monadic way.
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleAST
getModuleAST uri = (liftMaybe . moduleAST) =<< getEntry uri
