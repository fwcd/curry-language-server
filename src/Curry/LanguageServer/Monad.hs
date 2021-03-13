{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Curry.LanguageServer.Monad (
    LSState (..),
    defaultLSState, newLSStateVar,
    LSM,
    getLSState, putLSState, modifyLSState,
    getStore, putStore, modifyStore,
    runLSM
) where

import qualified Curry.LanguageServer.IndexStore as I
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Class
import Control.Monad.Trans (lift, liftIO)
import qualified Data.Map as M
import Language.LSP.Server
import qualified Language.LSP.Types      as J

-- The language server's state, e.g. holding loaded/compiled modules.
newtype LSState = LSState { indexStore :: I.IndexStore }

defaultLSState :: LSState
defaultLSState = LSState { indexStore = I.emptyStore }

newLSStateVar :: IO (MVar LSState)
newLSStateVar = newMVar defaultLSState

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

instance MonadState I.IndexStore LSM where
    get = getStore
    put = putStore

-- Fetches the language server's state inside the LSM monad
getLSState :: LSM LSState
getLSState = do
  stVar <- lift ask
  liftIO $ readMVar stVar

-- Replaces the language server's state inside the LSM monad
putLSState :: LSState -> LSM ()
putLSState s = do
  stVar <- lift ask
  liftIO $ putMVar stVar s

-- Updates the language server's state inside the LSM monad
modifyLSState :: (LSState -> LSState) -> LSM ()
modifyLSState m = do
  stVar <- lift ask
  liftIO $ modifyMVar stVar $ \s -> return (m s, ())

-- Fetches the index store holding compiled modules
getStore :: LSM I.IndexStore
getStore = indexStore <$> getLSState

-- Replaces the index store holding compiled modules
putStore :: I.IndexStore -> LSM ()
putStore i = modifyLSState $ \s -> s { indexStore = i }

-- Updates the index store holding compiled modules
modifyStore :: (I.IndexStore -> I.IndexStore) -> LSM ()
modifyStore m = modifyLSState $ \s -> s { indexStore = m $ indexStore s }

-- Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar
