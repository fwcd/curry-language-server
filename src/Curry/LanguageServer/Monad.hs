{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Curry.LanguageServer.Monad (
    LSState (..),
    newLSStateVar,
    LSM,
    getLSState, putLSState, modifyLSState,
    getStore, putStore, modifyStore,
    getDebouncers, putDebouncers, modifyDebouncers,
    runLSM
) where

import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.IndexStore as I
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (lift, liftIO)
import Curry.LanguageServer.Utils.Concurrent (ConstDebouncer)
import Data.Default (Default(..))
import qualified Data.Map as M
import Language.LSP.Server (LspT, LanguageContextEnv, runLspT)
import qualified Language.LSP.Types as J

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState { lssIndexStore :: I.IndexStore
                       , lssDebouncers :: M.Map J.Uri (ConstDebouncer IO)
                       }

instance Default LSState where
  def = LSState { lssIndexStore = I.emptyStore, lssDebouncers = M.empty }

newLSStateVar :: IO (MVar LSState)
newLSStateVar = newMVar def

-- | The monad holding (thread-safe) state used by the language server.
type LSM = LspT CFG.Config (ReaderT (MVar LSState) IO)

instance MonadState I.IndexStore LSM where
    get = getStore
    put = putStore

-- | Fetches the language server's state inside the LSM monad
getLSState :: LSM LSState
getLSState = do
  stVar <- lift ask
  liftIO $ readMVar stVar

-- | Replaces the language server's state inside the LSM monad
putLSState :: LSState -> LSM ()
putLSState s = do
  stVar <- lift ask
  liftIO $ putMVar stVar s

-- | Updates the language server's state inside the LSM monad
modifyLSState :: (LSState -> LSState) -> LSM ()
modifyLSState m = do
  stVar <- lift ask
  liftIO $ modifyMVar stVar $ \s -> return (m s, ())

-- | Fetches the index store holding compiled modules.
getStore :: LSM I.IndexStore
getStore = lssIndexStore <$> getLSState

-- | Replaces the index store holding compiled modules.
putStore :: I.IndexStore -> LSM ()
putStore i = modifyLSState $ \s -> s { lssIndexStore = i }

-- | Updates the index store holding compiled modules.
modifyStore :: (I.IndexStore -> I.IndexStore) -> LSM ()
modifyStore m = modifyLSState $ \s -> s { lssIndexStore = m $ lssIndexStore s }

-- | Fetches the debouncers for updating the index store.
getDebouncers :: LSM (M.Map J.Uri (ConstDebouncer IO))
getDebouncers = lssDebouncers <$> getLSState

-- | Replaces the debouncers for updating the index store.
putDebouncers :: M.Map J.Uri (ConstDebouncer IO) -> LSM ()
putDebouncers d = modifyLSState $ \s -> s { lssDebouncers = d }

-- | Updates the debouncers for updating the index store.
modifyDebouncers :: (M.Map J.Uri (ConstDebouncer IO) -> M.Map J.Uri (ConstDebouncer IO)) -> LSM ()
modifyDebouncers f = modifyLSState $ \s -> s { lssDebouncers = f $ lssDebouncers s }

-- | Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv CFG.Config -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar
