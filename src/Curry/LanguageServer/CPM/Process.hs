{-# LANGUAGE NumericUnderscores #-}

module Curry.LanguageServer.CPM.Process
    ( invokeCPM
    , CM (..)
    ) where

import Control.Exception (try, IOException)
import Control.Monad (when, fail, join)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.CPM.Monad
import Curry.LanguageServer.Utils.General (replaceString)
import Data.Either.Combinators (mapLeft)
import Data.Either.Extra (maybeToEither)
import Data.List.Extra (trim)
import System.Exit (ExitCode (..))
import System.FilePath (FilePath)
import System.Process
import System.Timeout (timeout)

-- | Invokes the Curry Package Manager executable (assuming it is on PATH) with the specified args.
invokeCPM :: FilePath -> [String] -> CM String
invokeCPM dir args = cm $ fmap (join . mapLeft errMessage) $ (try :: IO a -> IO (Either IOException a)) $ runCM $ do
    let action = readCreateProcessWithExitCode procOpts ""

    (exitCode, out, err) <- cm $ maybeToEither "CPM timed out!" <$> timeout microsecs action
    when (exitCode /= ExitSuccess) $ fail err

    return out
    where executableName = "cypm"
          errMessage e = "Please make sure that '" <> executableName <> "' is on your PATH! Error: " ++ replaceString "\n" " " (show e)
          procOpts = (proc executableName args) { cwd = Just dir }
          microsecs = 5_000_000
