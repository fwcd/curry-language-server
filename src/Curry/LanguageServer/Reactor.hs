module Curry.LanguageServer.Reactor (ReactorInput (HandlerRequest), reactor) where

import Control.Concurrent.STM.TChan
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types

newtype ReactorInput = HandlerRequest FromClientMessage

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf rin = do
    return () -- TODO
