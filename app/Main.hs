{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.STM
import Curry.LanguageServer.Config
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Options
import Curry.LanguageServer.Reactor
import qualified Data.Aeson as A
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import GHC.Conc
import qualified Language.LSP.Control as Ctrl
import qualified Language.LSP.Core as Core
import qualified Language.LSP.Types as J
import System.Exit
import qualified System.Log.Logger as L

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

main :: IO ()
main = runLanguageServer >>= \case
    0 -> exitSuccess
    c -> exitWith $ ExitFailure c

runLanguageServer :: IO Int
runLanguageServer = flip E.catches exceptHandlers $ do
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    let onStartup lf = do logs INFO "Starting reactor..."
                          labelledForkIO "Reactor" $ flip E.catches (void <$> exceptHandlers) $ reactor lf rin
                          return Nothing
        initializeCallbacks = Core.InitializeCallbacks { Core.onInitialConfiguration = resultToEither . extractInitialConfig,
                                                         Core.onConfigurationChange = resultToEither . extractChangedConfig,
                                                         Core.onStartup = onStartup }
        -- sessionLogFile = Just ".curry/.language-server/session.log"
        sessionLogFile = Nothing
    
    removeAllLogHandlers
    flip E.finally removeAllLogHandlers $ do
        -- Core.setupLogger (Just ".curry/.language-server/language-server.log") [] DEBUG
        Ctrl.run initializeCallbacks (lspHandlers rin) lspOptions sessionLogFile

    where exceptHandlers = [E.Handler ioExcept, E.Handler someExcept]
          ioExcept (e :: E.IOException) = print e >> logs ERROR (show e) >> return 1
          someExcept (e :: E.SomeException) = print e >> logs ERROR (show e) >> return 1
          extractInitialConfig :: J.InitializeRequest -> A.Result Config
          extractInitialConfig (J.RequestMessage _ _ _ p) = maybe (A.Success def) A.fromJSON $ J._initializationOptions p
          extractChangedConfig :: J.DidChangeConfigurationNotification -> A.Result Config
          extractChangedConfig (J.NotificationMessage _ _ (J.DidChangeConfigurationParams p)) = A.fromJSON p
          resultToEither :: A.Result a -> Either T.Text a
          resultToEither (A.Error e) = Left $ T.pack e
          resultToEither (A.Success s) = Right s
          labelledForkIO :: String -> IO () -> IO ()
          labelledForkIO label f = forkIO f >>= flip labelThread label
