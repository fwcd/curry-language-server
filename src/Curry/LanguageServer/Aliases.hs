module Curry.LanguageServer.Aliases (ReactorInput(..), RM) where

import Control.Monad.Reader
import Curry.LanguageServer.Config
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types

-- | The input to the reactor.
newtype ReactorInput = HandlerRequest FromClientMessage

-- | The reactor monad. Stores a config object and
-- a value.
type RM a = ReaderT (Core.LspFuncs Config) IO a
