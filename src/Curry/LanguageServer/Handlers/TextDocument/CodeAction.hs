{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.CodeAction (codeActionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Uri, currySpanInfo2Range, ppToText)
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
        lift $ fetchCodeActions range entry
    responder $ Right $ J.InL $ J.InR <$> fromMaybe [] actions

fetchCodeActions :: (MonadIO m, MonadLsp CFG.Config m) => J.Range -> I.ModuleStoreEntry -> m [J.CodeAction]
fetchCodeActions range entry = do
    actions <- maybe (pure []) (codeActions range) entry.moduleAST
    debugM $ "Found " <> T.pack (show (length actions)) <> " code action(s)"
    return actions

class HasCodeActions s where
    codeActions :: MonadIO m => J.Range -> s -> m [J.CodeAction]

instance HasCodeActions (CS.Module (Maybe CT.PredType)) where
    codeActions range mdl@(CS.Module spi _ _ _ _ _ _) = do
        maybeUri <- runMaybeT (currySpanInfo2Uri spi)

        -- TODO: Attach diagnostics, ideally the frontend could emit these
        --       quick fixes along with the warning messages?

        let typeHintActions = do
                (spi', i, tp) <- untypedTopLevelDecls mdl
                t <- maybeToList tp
                range' <- maybeToList $ currySpanInfo2Range spi'
                guard $ rangeOverlaps range range'
                uri <- maybeToList maybeUri
                -- TODO: Move the command identifier ('decl.applyTypeHint') to some
                --       central place to avoid repetition.
                let text = ppToText i <> " :: " <> ppToText t
                    args = [A.toJSON uri, A.toJSON $ range' ^. J.start, A.toJSON text]
                    command = J.Command text "decl.applyTypeHint" $ Just args
                    caKind = J.CodeActionKind_QuickFix
                    isPreferred = True
                    lens = J.CodeAction ("Add type annotation '" <> text <> "'") (Just caKind) Nothing (Just isPreferred) Nothing Nothing (Just command) Nothing
                return lens

        return typeHintActions

