module Curry.LanguageServer.Handlers (handlers) where

import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J

handlers :: S.Handlers LSM
handlers = mconcat []
