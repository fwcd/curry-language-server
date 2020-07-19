module Curry.LanguageServer.CPM.Process (
    CM (..)
) where

import Control.Monad (when, fail)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.CPM.Monad
import System.Exit (ExitCode (..))
import System.Process

-- Invokes the Curry Package Manager executable (assuming it is on PATH)
-- with the specified args.
invokeCPM :: [String] -> CM String
invokeCPM args = do
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "cypm" args ""
    when (exitCode /= ExitSuccess) $ fail err
    return out
