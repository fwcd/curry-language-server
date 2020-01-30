module Curry.LanguageServer.Handlers (lspHandlers) where

import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Curry.LanguageServer.Reactor (ReactorInput (HandlerRequest))
import Data.Default
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin = def { Core.initializedHandler = Just $ passHandler rin NotInitialized,
                        Core.renameHandler = Just $ passHandler rin ReqRename,
                        Core.hoverHandler = Just $ passHandler rin ReqHover,
                        Core.didOpenTextDocumentNotificationHandler = Just $ passHandler rin NotDidOpenTextDocument,
                        Core.didSaveTextDocumentNotificationHandler = Just $ passHandler rin NotDidSaveTextDocument,
                        Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument,
                        Core.didCloseTextDocumentNotificationHandler = Just $ passHandler rin NotDidCloseTextDocument,
                        Core.cancelNotificationHandler = Just $ passHandler rin NotCancelRequestFromClient,
                        Core.responseHandler = Just $ responseHandlerCb rin,
                        Core.codeActionHandler = Just $ passHandler rin ReqCodeAction,
                        Core.executeCommandHandler = Just $ passHandler rin ReqExecuteCommand }

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = atomically $ writeTChan rin $ HandlerRequest $ c notification

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin response = U.logs $ "*** Got ResponseMessage, ignoring: " ++ show response
