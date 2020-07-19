module Curry.LanguageServer.CPM.Process
    ( invokeCPM
    , CM (..)
    ) where

import Control.Exception (try, SomeException)
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
    let executableName = "cypm"
    res <- liftIO $ try $ readCreateProcessWithExitCode (proc executableName args) { cwd = Just dir } ""

    case res of
        Left e -> fail $ show (e :: SomeException) <> " (Please make sure that '" <> executableName <> "' is on your PATH!)"
        Right (exitCode, out, err) -> do
            when (exitCode /= ExitSuccess) $ fail err
            return out
