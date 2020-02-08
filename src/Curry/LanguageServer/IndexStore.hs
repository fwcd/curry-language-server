{-# LANGUAGE FlexibleContexts #-}
module Curry.LanguageServer.IndexStore (
    IndexStoreEntry (..),
    IndexStore,
    emptyStore,
    storedCount,
    storedEntry,
    storedEntries,
    addWorkspaceDir,
    recompileEntry,
    getCount,
    getEntry,
    getEntries,
    getModuleAST
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Message as CM
import qualified Curry.Files.Filenames as CF
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
import Data.Default
import Data.List (delete)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Language.Haskell.LSP.Types as J
import System.FilePath

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data IndexStoreEntry = IndexStoreEntry { moduleAST :: Maybe ModuleAST,
                                         compilerEnv :: Maybe CE.CompilerEnv,
                                         errorMessages :: [CM.Message],
                                         warningMessages :: [CM.Message],
                                         workspaceDir :: Maybe FilePath }

-- | An in-memory map containing the parsed ASTs.
type IndexStore = M.Map J.NormalizedUri IndexStoreEntry

instance Default IndexStoreEntry where
    def = IndexStoreEntry { moduleAST = Nothing, compilerEnv = Nothing, warningMessages = [], errorMessages = [], workspaceDir = Nothing }

-- | Fetches an empty index store.
emptyStore :: IndexStore
emptyStore = M.empty

-- | Fetches the number of stored entries.
storedCount :: IndexStore -> Int
storedCount = M.size

-- | Fetches the given entry in the store.
storedEntry :: J.NormalizedUri -> IndexStore -> Maybe IndexStoreEntry
storedEntry = M.lookup

-- | Fetches the entries in the store as a list.
storedEntries :: IndexStore -> [(J.NormalizedUri, IndexStoreEntry)]
storedEntries = M.toList

-- | Compiles the given directory recursively and stores its entries.
addWorkspaceDir :: (MonadState IndexStore m, MonadIO m) => Config -> FilePath -> m ()
addWorkspaceDir cfg dirPath = void $ runMaybeT $ do
    files <- liftIO $ walkCurrySourceFiles dirPath
    sequence $ recompileFile cfg (Just dirPath) <$> files

-- | Recompiles the entry with the given URI and stores the output.
recompileEntry :: (MonadState IndexStore m, MonadIO m) => Config -> J.NormalizedUri -> m ()
recompileEntry cfg uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    recompileFile cfg Nothing filePath

-- | Finds all Curry source files in a directory.
walkCurrySourceFiles :: FilePath -> IO [FilePath]
walkCurrySourceFiles = (filter ((== ".curry") . takeExtension) <$>) . walkFiles

-- | Recompiles the entry with its dependencies using explicit paths and stores the output.
recompileFile :: (MonadState IndexStore m, MonadIO m) => Config -> Maybe FilePath -> FilePath -> m ()
recompileFile cfg dirPath filePath = void $ do
    liftIO $ logs INFO $ "(Re-)compiling file " ++ takeFileName filePath
    let outDirPath = CF.currySubdir </> ".language-server"
        importPaths = [outDirPath]
    result <- liftIO $ C.compileCurryFileWithDeps cfg importPaths outDirPath filePath
    
    m <- get
    let uri = J.toNormalizedUri $ J.filePathToUri filePath
        previous :: J.NormalizedUri -> IndexStoreEntry
        previous = flip (M.findWithDefault $ def { workspaceDir = dirPath }) m
    case result of
        Left errs -> modify $ M.insert uri
                            $ (previous uri) { errorMessages = errs, warningMessages = [] }
        Right (o, warns) -> put $ flip insertAll m
                                $ (\(fp, ast) -> let u = J.toNormalizedUri $ J.filePathToUri fp
                                                 in (u, (previous u) { moduleAST = Just ast,
                                                                       compilerEnv = Just env,
                                                                       errorMessages = [],
                                                                       warningMessages = M.findWithDefault [] (Just u) ws }))
                               <$> asts
            where env = C.compilerEnv o
                  asts = C.moduleASTs o
                  ws = groupIntoMapBy ((J.toNormalizedUri <$>) . (curryPos2Uri =<<) . CM.msgPos) warns

-- | Fetches the number of entries in the store in a monadic way.
getCount :: (MonadState IndexStore m) => m Int
getCount = storedCount <$> get

-- | Fetches an entry in the store in a monadic way.
getEntry :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m IndexStoreEntry
getEntry uri = liftMaybe =<< storedEntry uri <$> get

-- | Fetches the entries in the store as a list in a monadic way.
getEntries :: (MonadState IndexStore m) => m [(J.NormalizedUri, IndexStoreEntry)]
getEntries = storedEntries <$> get

-- | Fetches the AST for a given URI in the store in a monadic way.
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleAST
getModuleAST uri = (liftMaybe . moduleAST) =<< getEntry uri
