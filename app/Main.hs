{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Server
import qualified Language.LSP.Types as J
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Monad (runLSM, newLSStateVar)

main :: IO ()
main = runLanguageServer >>= \case
    0 -> exitSuccess
    c -> exitWith $ ExitFailure c

runLanguageServer :: IO Int
runLanguageServer = do
    state <- newLSStateVar
    S.runServer $ S.ServerDefinition
        { onConfigurationChange = const $ pure $ Right ()
        , onInitialize = const . pure . Right
        , staticHandlers = handlers flags
        , interpretHandler = \env -> S.Iso (\lsm -> runLSM lsm state env) liftIO
        , options = defaultOptions { textDocumentSync = Just syncOptions }
        }
    where
        syncOptions = J.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just J.TdSyncIncremental) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ J.InR $ J.SaveOptions $ Just False) -- save
