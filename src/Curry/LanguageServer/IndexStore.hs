{-# LANGUAGE FlexibleContexts #-}
module Curry.LanguageServer.IndexStore (
    IndexStore (..),
    emptyStore,
    recompileEntry,
    getEntry,
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

-- | Recompiles the entry, stores the output in the AST
recompileEntry :: (MonadState IndexStore m, MonadIO m) => J.NormalizedUri -> IndexStore -> m ()
recompileEntry uri s = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    result <- liftIO $ C.compileCurry [] filePath -- TODO: Use proper import path

    previous <- M.findWithDefault def uri <$> get
    let entry = case result of
                    Left errs -> previous { errorMessages = errs }
                    Right (o, warns) -> previous { moduleAST = Just $ C.moduleAST o,
                                                   compilerEnv = Just $ C.compilerEnv o,
                                                   warningMessages = warns }
    modify $ M.insert uri entry

-- | Fetches an entry in the store.
getEntry :: (MonadState IndexStore m) => J.NormalizedUri -> m (Maybe IndexStoreEntry)
getEntry uri = M.lookup uri <$> get

-- | Fetches the AST for a given URI in the store.
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> m (Maybe C.ModuleAST)
getModuleAST uri = (moduleAST =<<) <$> getEntry uri
