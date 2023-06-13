module Curry.LanguageServer.CPM.Monad (CPMM, cpmm, runCPMM) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)

-- | The monad for running the Curry Package Manager process.
type CPMM = ExceptT String IO

-- | Runs the monad used for running Curry Package Manager actions.
runCPMM :: MonadIO m => CPMM a -> m (Either String a)
runCPMM = liftIO . runExceptT

-- | Constructs the monad used for running the Curry Package Manager actions.
cpmm :: IO (Either String a) -> CPMM a
cpmm = ExceptT
