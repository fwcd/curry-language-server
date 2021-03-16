{-# LANGUAGE LambdaCase, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Default (Default (..))
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Handlers.Command (commands)
import Curry.LanguageServer.Monad (runLSM, newLSStateVar)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = runLanguageServer >>= \case
    0 -> exitSuccess
    c -> exitWith $ ExitFailure c

runLanguageServer :: IO Int
runLanguageServer = do
    state <- newLSStateVar
    S.runServer $ S.ServerDefinition
        -- TODO: The most recent (unreleased 1.1.x) version of the LSP library
        --       updates this config handling and so should we.
        { S.onConfigurationChange = const $ pure $ Right def
        , S.doInitialize = const . pure . Right
        , S.staticHandlers = handlers
        , S.interpretHandler = \env -> S.Iso (\lsm -> runLSM lsm state env) liftIO
        , S.options = S.defaultOptions
            { S.textDocumentSync = Just syncOptions
            , S.executeCommandCommands = Just $ fst <$> commands
            , S.serverInfo = Just $ J.ServerInfo "Curry Language Server" Nothing
            }
        }
    where
        syncOptions = J.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just J.TdSyncIncremental) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ J.InR $ J.SaveOptions $ Just False) -- save
