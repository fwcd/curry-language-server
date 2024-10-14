module Curry.LanguageServer.Handlers (handlers) where

import Curry.LanguageServer.Handlers.Cancel (cancelHandler)
import Curry.LanguageServer.Handlers.Initialize (initializedHandler)
import Curry.LanguageServer.Handlers.TextDocument.CodeAction (codeActionHandler)
import Curry.LanguageServer.Handlers.TextDocument.CodeLens (codeLensHandler)
import Curry.LanguageServer.Handlers.TextDocument.Completion (completionHandler)
import Curry.LanguageServer.Handlers.TextDocument.Definition (definitionHandler)
import Curry.LanguageServer.Handlers.TextDocument.DocumentSymbol (documentSymbolHandler)
import Curry.LanguageServer.Handlers.TextDocument.Notifications (didOpenHandler, didChangeHandler, didSaveHandler, didCloseHandler)
import Curry.LanguageServer.Handlers.TextDocument.Hover (hoverHandler)
import Curry.LanguageServer.Handlers.TextDocument.SignatureHelp (signatureHelpHandler)
import Curry.LanguageServer.Handlers.Workspace.Command (executeCommandHandler)
import Curry.LanguageServer.Handlers.Workspace.Notifications (didChangeConfigurationHandler)
import Curry.LanguageServer.Handlers.Workspace.Symbol (workspaceSymbolHandler)
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Server as S

handlers :: J.ClientCapabilities -> S.Handlers LSM
handlers _caps = mconcat
    [ -- Request handlers
      completionHandler
    , executeCommandHandler
    , definitionHandler
    , documentSymbolHandler
    , hoverHandler
    , workspaceSymbolHandler
    , codeActionHandler
    , codeLensHandler
    , signatureHelpHandler
      -- Notification handlers
    , initializedHandler
    , didOpenHandler
    , didChangeHandler
    , didSaveHandler
    , didCloseHandler
    , didChangeConfigurationHandler
    , cancelHandler
    ]
