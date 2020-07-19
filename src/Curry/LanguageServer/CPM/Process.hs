module Curry.LanguageServer.CPM.Process
    ( invokeCPM
    , CM (..)
    ) where

import Control.Monad (when, fail)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.CPM.Monad
import System.Exit (ExitCode (..))
import System.FilePath (FilePath)
import System.Process

-- Invokes the Curry Package Manager executable (assuming it is on PATH)
-- with the specified args.
invokeCPM :: FilePath -> [String] -> CM String
invokeCPM dir args = do
    (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode (proc "cpm" args) { cwd = Just dir } ""
    when (exitCode /= ExitSuccess) $ fail err
    return out
