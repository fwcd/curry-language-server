module Curry.LanguageServer.CPM.Process
    ( invokeCPM
    , CM (..)
    ) where

import Control.Exception (try, SomeException)
import Control.Monad (when, fail)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.CPM.Monad
import Data.Either.Combinators (mapLeft)
import System.Exit (ExitCode (..))
import System.FilePath (FilePath)
import System.Process

-- | Invokes the Curry Package Manager executable (assuming it is on PATH) with the specified args.
invokeCPM :: FilePath -> [String] -> CM String
invokeCPM dir args = do
    let executableName = "cypm"
        errMessage = "Please make sure that '" <> executableName <> "' is on your PATH!"
        procOpts = (proc executableName args) { cwd = Just dir }
    
    (exitCode, out, err) <- cm $ mapLeft (const errMessage) <$> ((try $ readCreateProcessWithExitCode procOpts "") :: IO (Either SomeException (ExitCode, String, String)))
    when (exitCode /= ExitSuccess) $ fail err
    return out
