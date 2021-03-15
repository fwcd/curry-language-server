{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Monad (runLSM, newLSStateVar)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.Log.Logger

main :: IO ()
main = runLanguageServer >>= \case
    0 -> exitSuccess
    c -> exitWith $ ExitFailure c

runLanguageServer :: IO Int
runLanguageServer = do
    state <- newLSStateVar
    S.runServer $ S.ServerDefinition
        { S.defaultConfig = def
        , S.onConfigurationChange = \_old v -> case A.fromJSON v of
                                            A.Error e -> Left $ T.pack e
                                            A.Success cfg -> Right (cfg :: CFG.Config)
        , S.doInitialize = const . pure . Right
        , S.staticHandlers = handlers
        , S.interpretHandler = \env -> S.Iso (\lsm -> runLSM lsm state env) liftIO
        , S.options = S.defaultOptions { S.textDocumentSync = Just syncOptions }
        }
    where
        syncOptions = J.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just J.TdSyncIncremental) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ J.InR $ J.SaveOptions $ Just False) -- save
