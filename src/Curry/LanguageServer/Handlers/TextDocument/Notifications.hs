{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.TextDocument.Notifications
    ( didOpenHandler
    , didChangeHandler
    , didSaveHandler
    , didCloseHandler
    ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (markModuleDirty, LSM)
import Curry.LanguageServer.Utils.Logging (debugM)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J

didOpenHandler :: S.Handlers LSM
didOpenHandler = S.notificationHandler J.STextDocumentDidOpen $ \nt -> do
    debugM "Processing open notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didChangeHandler :: S.Handlers LSM
didChangeHandler = S.notificationHandler J.STextDocumentDidChange $ \nt -> do
    debugM "Processing change notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didSaveHandler :: S.Handlers LSM
didSaveHandler = S.notificationHandler J.STextDocumentDidSave $ \nt -> do
    debugM "Processing save notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didCloseHandler :: S.Handlers LSM
didCloseHandler = S.notificationHandler J.STextDocumentDidClose $ \_nt -> do
    debugM "Processing close notification"
    -- TODO: Remove file from LSM state?

-- | Schedules recompilation by marking the module as dirty.
updateIndexStoreDebounced :: J.Uri -> LSM ()
updateIndexStoreDebounced uri = do
    debugM $ "Scheduling recompilation for " <> T.pack (show uri)
    markModuleDirty uri $ updateIndexStore uri

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> LSM ()
updateIndexStore uri = void $ runMaybeT $ do
    fl <- lift fileLoader
    cfg <- lift S.getConfig
    normUri <- normalizeUriWithPath uri
    lift $ I.recompileModule cfg fl normUri
    entry <- I.getModule normUri
    lift $ emitDiagnostics normUri entry

