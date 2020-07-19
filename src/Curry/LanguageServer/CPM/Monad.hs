module Curry.LanguageServer.CPM.Monad (CM) where

import Control.Monad.Trans.Except

-- The monad for running the Curry Package Manager process.
type CM a = ExceptT String IO a

-- Runs the monad used for running Curry Package Manager actions.
runCM :: CM a -> IO (Either String a)
runCM = runExceptT
