{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.CodeLens (codeLensHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (LSM, scheduleModuleHandler)
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range, currySpanInfo2Uri, ppToText)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
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

codeLensHandler :: S.Handlers LSM
codeLensHandler = S.requestHandler J.SMethod_TextDocumentCodeLens $ \req responder -> do
    debugM "Processing code lens request"
    let J.CodeLensParams _ _ doc = req ^. J.params
        uri = doc ^. J.uri
    normUri <- normalizeUriWithPath uri

    debugM $ "Scheduling code lenses for " <> T.pack (show uri)
    scheduleModuleHandler uri $ do
        lenses <- runMaybeT $ do
            entry <- I.getModule normUri
            lift $ fetchCodeLenses entry
        responder $ Right $ J.InL $ fromMaybe [] lenses

fetchCodeLenses :: (MonadIO m, MonadLsp CFG.Config m) => I.ModuleStoreEntry -> m [J.CodeLens]
fetchCodeLenses entry = do
    lenses <- maybe (pure []) codeLenses entry.moduleAST
    infoM $ "Found " <> T.pack (show (length lenses)) <> " code lens(es)"
    return lenses

class HasCodeLenses s where
    codeLenses :: MonadIO m => s -> m [J.CodeLens]

instance HasCodeLenses (CS.Module (Maybe CT.PredType)) where
    codeLenses mdl@(CS.Module spi _ _ _ _ _ _) = do
        maybeUri <- liftIO $ runMaybeT (currySpanInfo2Uri spi)

        let typeHintLenses = do
                (spi', i, tp) <- untypedTopLevelDecls mdl
                t <- maybeToList tp
                range <- maybeToList $ currySpanInfo2Range spi'
                uri <- maybeToList maybeUri
                -- TODO: Move the command identifier ('decl.applyTypeHint') to some
                --       central place to avoid repetition.
                let text = ppToText i <> " :: " <> ppToText t
                    args = [A.toJSON uri, A.toJSON $ range ^. J.start, A.toJSON text]
                    command = J.Command text "decl.applyTypeHint" $ Just args
                    lens = J.CodeLens range (Just command) Nothing
                return lens

        return typeHintLenses
