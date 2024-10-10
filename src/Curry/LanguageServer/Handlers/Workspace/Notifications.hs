{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Workspace.Notifications
    ( didChangeConfigurationHandler
    ) where

import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Logging (debugM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Message as J

didChangeConfigurationHandler :: S.Handlers LSM
didChangeConfigurationHandler = S.notificationHandler J.SMethod_WorkspaceDidChangeConfiguration $ \_nt -> do
    debugM "Processing configuration change notification"
    -- TODO
    
