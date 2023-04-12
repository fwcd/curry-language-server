module Curry.LanguageServer.Handlers.Cancel
    ( cancelHandler
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.Log.Logger (debugM)

cancelHandler :: S.Handlers LSM
cancelHandler = S.notificationHandler J.SCancelRequest $ \_nt -> do
    liftIO $ debugM "cls.workspace.notifications" "Processing cancel request"
    -- TODO: This is currently just a stub to prevent error messages
    --       about the unimplemented request from showing up, we might
    --       want to implement actual cancellation (though most things
    --       are handled synchronously currently, so it shouldn't really
    --       be needed yet)
    
