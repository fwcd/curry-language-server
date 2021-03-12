module Curry.LanguageServer.Monad (
    LSState (..),
    defaultLSState, newLSStateVar,
    LSM,
    getLSState, putLSState, modifyLSState,
    getLoaded, putLoaded, modifyLoaded,
    runLSM
) where

import qualified Curry.LanguageServer.IndexStore as I
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.Map as M
import Language.LSP.Server
import qualified Language.LSP.Types      as J

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState { indexStore :: I.IndexStore }

defaultLSState :: LSState
defaultLSState = LSState { indexStore = I.emptyStore }

newLSStateVar :: IO (MVar LSState)
newLSStateVar = newMVar defaultLSState

-- The monad holding (thread-safe) state used by the language server.
type LSM = LspT () (ReaderT (MVar LSState) IO)

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

-- Fetches the loaded state holding compiled modules
getLoaded :: LSM (M.Map J.NormalizedUri Loaded)
getLoaded = lsLoaded <$> getLSState

-- Replaces the loaded state holding compiled modules
putLoaded :: M.Map J.NormalizedUri Loaded -> LSM ()
putLoaded l = modifyLSState $ \s -> s { lsLoaded = l }

-- Updates the loaded state holding compiled modules
modifyLoaded :: (M.Map J.NormalizedUri Loaded -> M.Map J.NormalizedUri Loaded) -> LSM ()
modifyLoaded m = modifyLSState $ \s -> s { lsLoaded = m $ lsLoaded s }

-- Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar
