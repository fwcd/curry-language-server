{-# LANGUAGE FlexibleContexts #-}
module Curry.LanguageServer.IndexStore (
    IndexStoreEntry (..),
    IndexStore,
    emptyStore,
    storedCount,
    storedEntry,
    storedEntries,
    compileWorkspace,
    recompileEntry,
    getCount,
    getEntry,
    getEntries,
    getModuleAST
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Message as CM
import qualified Curry.Syntax as CS
import qualified CompilerEnv as CE

import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.Utils.General
import Data.Default
import qualified Data.Map as M
import qualified Language.Haskell.LSP.Types as J
import System.FilePath

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data IndexStoreEntry = IndexStoreEntry { moduleAST :: Maybe C.ModuleAST,
                                         compilerEnv :: Maybe CE.CompilerEnv,
                                         warningMessages :: [CM.Message],
                                         errorMessages :: [CM.Message] }

-- | An in-memory map containing the parsed ASTs.
type IndexStore = M.Map J.NormalizedUri IndexStoreEntry

instance Default IndexStoreEntry where
    def = IndexStoreEntry { moduleAST = Nothing, compilerEnv = Nothing, warningMessages = [], errorMessages = [] }

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

-- | Compiles the entire workspace under the given directory into the store.
compileWorkspace :: (MonadState IndexStore m, MonadIO m) => FilePath -> m ()
compileWorkspace dirPath = void $ runMaybeT $ do
    files <- liftIO $ walkFiles dirPath
    sequence $ recompileEntry <$> J.toNormalizedUri <$> J.filePathToUri <$> files

-- | Recompiles the entry, stores the output in the AST
recompileEntry :: (MonadState IndexStore m, MonadIO m) => J.NormalizedUri -> m ()
recompileEntry uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    result <- liftIO $ C.compileCurry [] filePath -- TODO: Use proper import path

    previous <- M.findWithDefault def uri <$> get
    let entry = case result of
                    Left errs -> previous { errorMessages = errs }
                    Right (o, warns) -> previous { moduleAST = Just $ C.moduleAST o,
                                                   compilerEnv = Just $ C.compilerEnv o,
                                                   warningMessages = warns }
    modify $ M.insert uri entry

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
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m C.ModuleAST
getModuleAST uri = (liftMaybe . moduleAST) =<< getEntry uri
