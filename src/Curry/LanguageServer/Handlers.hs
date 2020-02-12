module Curry.LanguageServer.Handlers (lspHandlers) where

import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Reactor
import Data.Default
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin = def { -- Notifications from the client
                        Core.initializedHandler = Just $ passHandler rin NotInitialized,
                        Core.didChangeConfigurationParamsHandler = Just $ passHandler rin NotDidChangeConfiguration,
                        Core.didOpenTextDocumentNotificationHandler = Just $ passHandler rin NotDidOpenTextDocument,
                        Core.didSaveTextDocumentNotificationHandler = Just $ passHandler rin NotDidSaveTextDocument,
                        Core.didCloseTextDocumentNotificationHandler = Just $ passHandler rin NotDidCloseTextDocument,
                        Core.cancelNotificationHandler = Just $ passHandler rin NotCancelRequestFromClient,
                        -- Requests from the client
                        Core.renameHandler = Just $ passHandler rin ReqRename,
                        Core.hoverHandler = Just $ passHandler rin ReqHover,
                        Core.completionHandler = Just $ passHandler rin ReqCompletion,
                        Core.completionResolveHandler = Just $ passHandler rin ReqCompletionItemResolve,
                        Core.definitionHandler = Just $ passHandler rin ReqDefinition,
                        Core.documentSymbolHandler = Just $ passHandler rin ReqDocumentSymbols,
                        Core.workspaceSymbolHandler = Just $ passHandler rin ReqWorkspaceSymbols,
                        -- Responses
                        Core.responseHandler = Just $ responseHandlerCb rin }

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = atomically $ writeTChan rin $ c notification

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin response = logs NOTICE $ "*** Got ResponseMessage, ignoring: " ++ show response
