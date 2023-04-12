module Curry.LanguageServer.Handlers.Workspace.Notifications
    ( didChangeConfigurationHandler
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.Log.Logger (debugM)

didChangeConfigurationHandler :: S.Handlers LSM
didChangeConfigurationHandler = S.notificationHandler J.SWorkspaceDidChangeConfiguration $ \_nt -> do
    liftIO $ debugM "cls.workspace.notifications" "Processing configuration change notification"
    -- TODO
    
