{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.CodeAction (codeActionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Message as CM
import qualified Curry.Syntax as CS
import qualified Curry.Frontend.Base.Types as CT

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Uri, currySpanInfo2Range, curryQuickFix2CodeAction, ppToText, curryMsg2Diagnostic)
import Curry.LanguageServer.Utils.General (rangeOverlaps)
import Curry.LanguageServer.Utils.Logging (debugM)
import Curry.LanguageServer.Utils.Sema (untypedTopLevelDecls)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J

codeActionHandler :: S.Handlers LSM
codeActionHandler = S.requestHandler J.SMethod_TextDocumentCodeAction $ \req responder -> do
    debugM "Processing code action request"
    let J.CodeActionParams _ _ doc range _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- normalizeUriWithPath uri
    actions <- runMaybeT $ do
        entry <- I.getModule normUri
        lift $ fetchCodeActionsInRange range entry
    responder $ Right $ J.InL $ J.InR <$> fromMaybe [] actions

fetchCodeActionsInRange :: (MonadIO m, MonadLsp CFG.Config m) => J.Range -> I.ModuleStoreEntry -> m [J.CodeAction]
fetchCodeActionsInRange range entry = filterCodeActionsInRange range <$> fetchCodeActions entry

filterCodeActionsInRange :: J.Range -> [J.CodeAction] -> [J.CodeAction]
filterCodeActionsInRange range = filter $ \a -> any (rangeOverlaps range)
    [ txtEdit ^. J.range
    | Just workspaceEdit <- [a ^. J.edit]
    , Just changes <- [workspaceEdit ^. J.documentChanges]
    , J.InL docEdit <- changes
    , J.InL txtEdit <- docEdit ^. J.edits
    ]

fetchCodeActions :: (MonadIO m, MonadLsp CFG.Config m) => I.ModuleStoreEntry -> m [J.CodeAction]
fetchCodeActions entry = do
    let msgs = entry.warningMessages ++ entry.errorMessages
        diags = [curryMsg2Diagnostic s m | (s, ms) <- zip [J.DiagnosticSeverity_Warning, J.DiagnosticSeverity_Error] [entry.warningMessages, entry.errorMessages], m <- ms]
    mapMaybeM (runMaybeT . uncurry curryQuickFix2CodeAction) [(f, [diag]) | (diag, m) <- zip diags msgs, f <- CM.msgFixes m]
