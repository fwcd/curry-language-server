module Curry.LanguageServer.Handlers (handlers) where

import Curry.LanguageServer.Handlers.CodeAction (codeActionHandler)
import Curry.LanguageServer.Handlers.CodeLens (codeLensHandler)
import Curry.LanguageServer.Handlers.Command (commandHandler)
import Curry.LanguageServer.Handlers.Completion (completionHandler)
import Curry.LanguageServer.Handlers.Definition (definitionHandler)
import Curry.LanguageServer.Handlers.DocumentSymbols (documentSymbolHandler)
import Curry.LanguageServer.Handlers.Hover (hoverHandler)
import Curry.LanguageServer.Handlers.Initialized (initializedHandler)
import Curry.LanguageServer.Handlers.SemanticTokens (semanticTokensHandler)
import Curry.LanguageServer.Handlers.SignatureHelp (signatureHelpHandler)
import Curry.LanguageServer.Handlers.TextDocument (didOpenHandler, didChangeHandler, didSaveHandler, didCloseHandler)
import Curry.LanguageServer.Handlers.WorkspaceSymbols (workspaceSymbolHandler)
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S

handlers :: S.Handlers LSM
handlers = mconcat
    [ -- Request handlers
      completionHandler
    , commandHandler
    , definitionHandler
    , documentSymbolHandler
    , hoverHandler
    , workspaceSymbolHandler
    , codeActionHandler
    , codeLensHandler
    , signatureHelpHandler
    , semanticTokensHandler
      -- Notification handlers
    , initializedHandler
    , didOpenHandler
    , didChangeHandler
    , didSaveHandler
    , didCloseHandler
    ]
