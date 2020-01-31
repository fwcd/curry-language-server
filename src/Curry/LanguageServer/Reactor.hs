module Curry.LanguageServer.Reactor (ReactorInput (HandlerRequest), reactor) where

import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.STM
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Utility as U

newtype ReactorInput = HandlerRequest FromClientMessage

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf rin = do
    U.logs "reactor: entered"
    flip runReaderT lf $ forever $ do
        hreq <- liftIO $ atomically $ readTChan rin
        case hreq of
            HandlerRequest (RspFromClient rsp) -> do
                liftIO $ U.logs $ "reactor: RspFromClient " ++ show rsp
            
            HandlerRequest req -> do
                liftIO $ U.logs $ "reactor: Other HandlerRequest " ++ show req
