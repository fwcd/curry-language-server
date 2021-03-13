module Curry.LanguageServer.Handlers (handlers) where

import Curry.LanguageServer.Handlers.Completion (completionHandler)
import Curry.LanguageServer.Handlers.DocumentSymbols (documentSymbolHandler)
import Curry.LanguageServer.Handlers.Hover (hoverHandler)
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J

handlers :: S.Handlers LSM
handlers = mconcat
    [ completionHandler
    , documentSymbolHandler
    , hoverHandler
    ]
