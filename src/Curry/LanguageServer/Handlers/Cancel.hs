{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Cancel
    ( cancelHandler
    ) where

import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Logging (debugM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Message as J

cancelHandler :: S.Handlers LSM
cancelHandler = S.notificationHandler J.SMethod_CancelRequest $ \_nt -> do
    debugM "Processing cancel request"
    -- TODO: This is currently just a stub to prevent error messages
    --       about the unimplemented request from showing up, we might
    --       want to implement actual cancellation (though most things
    --       are handled synchronously currently, so it shouldn't really
    --       be needed yet)
    
