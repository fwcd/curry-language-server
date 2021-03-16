module Curry.LanguageServer.Handlers (handlers) where

import Curry.LanguageServer.Handlers.CodeLens (codeLensHandler)
import Curry.LanguageServer.Handlers.Completion (completionHandler)
import Curry.LanguageServer.Handlers.Definition (definitionHandler)
import Curry.LanguageServer.Handlers.DocumentSymbols (documentSymbolHandler)
import Curry.LanguageServer.Handlers.Hover (hoverHandler)
import Curry.LanguageServer.Handlers.Initialized (initializedHandler)
import Curry.LanguageServer.Handlers.TextDocument (didOpenHandler, didChangeHandler, didSaveHandler, didCloseHandler)
import Curry.LanguageServer.Handlers.WorkspaceSymbols (workspaceSymbolHandler)
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J

handlers :: S.Handlers LSM
handlers = mconcat
    [ -- Request handlers
      completionHandler
    , definitionHandler
    , documentSymbolHandler
    , hoverHandler
    , workspaceSymbolHandler
    , codeLensHandler
      -- Notification handlers
    , initializedHandler
    , didOpenHandler
    , didChangeHandler
    , didSaveHandler
    ]
