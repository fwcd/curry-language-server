{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad.STM
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Options
import Curry.LanguageServer.Reactor
import GHC.Conc
import qualified Language.Haskell.LSP.Control as Ctrl
import qualified Language.Haskell.LSP.Core as Core
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
    let onStartup lf = do labelledForkIO "Reactor" $ reactor lf rin
                          return Nothing
        initializeCallbacks = Core.InitializeCallbacks { Core.onInitialConfiguration = const $ Right (),
                                                         Core.onConfigurationChange = const $ Right (),
                                                         Core.onStartup = onStartup }

    flip E.finally finalProc $ do
        Core.setupLogger (Just "curry-language-server.log") [] L.DEBUG
        Ctrl.run initializeCallbacks (lspHandlers rin) lspOptions (Just "curry-language-server-session.log")

    where exceptHandlers = [E.Handler ioExcept, E.Handler someExcept]
          ioExcept (e :: E.IOException) = print e >> return 1
          someExcept (e :: E.SomeException) = print e >> return 1
          finalProc = L.removeAllHandlers

labelledForkIO :: String -> IO () -> IO ()
labelledForkIO label f = forkIO f >>= flip labelThread label
