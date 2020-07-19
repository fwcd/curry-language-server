module Curry.LanguageServer.CPM.Monad (CM, cm, runCM) where

import Control.Monad.Trans.Except

-- The monad for running the Curry Package Manager process.
type CM a = ExceptT String IO a

-- Runs the monad used for running Curry Package Manager actions.
runCM :: CM a -> IO (Either String a)
runCM = runExceptT

-- Constructs the monad used for running the Curry Package Manager actions.
cm :: IO (Either String a) -> CM a
cm = ExceptT
