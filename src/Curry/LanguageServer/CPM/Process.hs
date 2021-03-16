{-# LANGUAGE NumericUnderscores #-}

module Curry.LanguageServer.CPM.Process
    ( invokeCPM
    ) where

import Control.Exception (try, IOException)
import Control.Monad (when, join)
import Curry.LanguageServer.CPM.Monad
import Curry.LanguageServer.Utils.General (replaceString)
import Data.Either.Combinators (mapLeft)
import Data.Either.Extra (maybeToEither)
import System.Exit (ExitCode (..))
import System.Process
import System.Timeout (timeout)

-- | Invokes the Curry Package Manager executable with the specified args.
invokeCPM :: FilePath -> [String] -> FilePath -> CM String
invokeCPM dir args cpmPath = cm $ fmap (join . mapLeft errMessage) $ (try :: IO a -> IO (Either IOException a)) $ runCM $ do
    let action = readCreateProcessWithExitCode procOpts ""

    (exitCode, out, err) <- cm $ maybeToEither "CPM timed out!" <$> timeout microsecs action
    when (exitCode /= ExitSuccess) $ fail err

    return out
    where errMessage e = "Please make sure that '" <> cpmPath <> "' exists or is on your PATH! Error: " ++ replaceString "\n" " " (show e)
          procOpts = (shell $ unwords $ cpmPath : args) { cwd = Just dir }
          microsecs = 20_000_000
