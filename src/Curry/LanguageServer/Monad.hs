{-# LANGUAGE FlexibleInstances, NoFieldSelectors, OverloadedRecordDot, MultiParamTypeClasses, OverloadedStrings #-}
module Curry.LanguageServer.Monad
    ( LSState (..)
    , newLSStateVar
    , LSM
    , getLSState, putLSState, modifyLSState
    , getStore, putStore, modifyStore
    , triggerDebouncer
    , markModuleDirty, scheduleModuleHandler
    , runLSM
    ) where

import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar)
import Control.Monad.IO.Unlift (askRunInIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (lift, liftIO)
import Curry.LanguageServer.Utils.Concurrent (Debouncer, debounce)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Language.LSP.Server (LspT, LanguageContextEnv, runLspT)
import qualified Language.LSP.Protocol.Types as J

data DirtyModuleHandlers = DirtyModuleHandlers { recompileHandler :: IO ()
                                               , auxiliaryHandler :: IO ()
                                               }

instance Default DirtyModuleHandlers where
    def = DirtyModuleHandlers
        { recompileHandler = return ()
        , auxiliaryHandler = return ()
        }

-- The language server's state, e.g. holding loaded/compiled modules.
data LSState = LSState { indexStore :: I.IndexStore
                       , dirtyModuleHandlers :: M.Map J.Uri DirtyModuleHandlers
                       , debouncer :: Debouncer (IO ()) IO
                       }

newLSState :: IO LSState
newLSState = do
    -- TODO: Make this delay configurable, e.g. through a config option
    let delayMs = 500
    debouncer <- debounce (delayMs * 1000) id
    return LSState
        { indexStore = def
        , dirtyModuleHandlers = M.empty
        , debouncer = debouncer
        }

newLSStateVar :: IO (MVar LSState)
newLSStateVar = newMVar =<< newLSState

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
getStore = (.indexStore) <$> getLSState

-- | Replaces the index store holding compiled modules.
putStore :: I.IndexStore -> LSM ()
putStore i = modifyLSState $ \s -> s { indexStore = i }

-- | Updates the index store holding compiled modules.
modifyStore :: (I.IndexStore -> I.IndexStore) -> LSM ()
modifyStore m = modifyLSState $ \s -> s { indexStore = m s.indexStore }

-- | Updates the dirty module handlers for a module.
updateDirtyModuleHandlers :: J.Uri -> (DirtyModuleHandlers -> DirtyModuleHandlers) -> LSM ()
updateDirtyModuleHandlers uri f = modifyLSState $ \s -> s { dirtyModuleHandlers = M.alter (Just . f . fromMaybe def) uri s.dirtyModuleHandlers }

-- | Runs all dirty module handlers.
runDirtyModuleHandlers :: LSM ()
runDirtyModuleHandlers = do
    hs <- (.dirtyModuleHandlers) <$> getLSState
    liftIO $ M.foldl' (>>) (return ()) $ M.map (\dmh -> dmh.recompileHandler >> dmh.auxiliaryHandler) hs

-- | Clears all dirty module handlers.
clearDirtyModuleHandlers :: LSM ()
clearDirtyModuleHandlers = modifyLSState $ \s -> s { dirtyModuleHandlers = M.empty }

-- | Triggers the debouncer that (eventually) executes and removes all dirty module handlers.
triggerDebouncer :: LSM ()
triggerDebouncer = do
    (db, _) <- (.debouncer) <$> getLSState
    runInIO <- askRunInIO
    liftIO $ db $ runInIO $ do
        runDirtyModuleHandlers
        clearDirtyModuleHandlers

-- | Marks a module as dirty (= edited, but not compiled yet) and replaces the recompilation handler with the given handler.
markModuleDirty :: J.Uri -> LSM () -> LSM ()
markModuleDirty uri h = do
    runInIO <- askRunInIO
    updateDirtyModuleHandlers uri $ \dmh -> dmh { recompileHandler = runInIO h }
    triggerDebouncer

-- | Adds a handler that either executes directly if the module is clean (= compiled, unedited) or defers its execution to the next compilation.
scheduleModuleHandler :: J.Uri -> LSM () -> LSM ()
scheduleModuleHandler uri h = do
    hs <- (.dirtyModuleHandlers) <$> getLSState
    if M.member uri hs then do
        -- Module is dirty (edited since the last compilation), defer execution by attaching it as an auxiliary handler
        runInIO <- askRunInIO
        updateDirtyModuleHandlers uri $ \dmh -> dmh { auxiliaryHandler = dmh.auxiliaryHandler >> runInIO h }
        triggerDebouncer
    else do
        -- Module is clean (unedited since the last compilation), execute directly
        h

-- | Runs the language server's state monad.
runLSM :: LSM a -> MVar LSState -> LanguageContextEnv CFG.Config -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar
