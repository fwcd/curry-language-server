{-# LANGUAGE FlexibleContexts, OverloadedStrings, NoFieldSelectors, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.References (referencesHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.SpanInfo as CSPI

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Monad (LSM, getStore)
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Location)
import Curry.LanguageServer.Utils.General (liftMaybe, (<.$>), joinFst)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Syntax (HasQualIdentifiers (..), HasExpressions (expressions), HasQualIdentifier (qualIdentifier))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Index.Resolve (resolveAtPos, resolveQualIdent)
import Curry.LanguageServer.Index.Symbol (Symbol (..))
import qualified Curry.LanguageServer.Index.Store as I
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Types as J

-- DEBUG
import Debug.Trace

referencesHandler :: S.Handlers LSM
referencesHandler = S.requestHandler J.SMethod_TextDocumentReferences $ \req responder -> do
    debugM "Processing references request"
    let pos = req ^. J.params . J.position
        uri = req ^. J.params . J.textDocument . J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    refs <- (fromMaybe [] <$>) . runMaybeT $ do
        lift $ debugM $ "Looking up " <> J.getUri (J.fromNormalizedUri normUri) <> " in " <> T.pack (show (M.keys store.modules))
        entry <- I.getModule normUri
        lift $ fetchReferences store entry pos
    responder $ Right $ J.InL refs

fetchReferences :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> I.ModuleStoreEntry -> J.Position -> m [J.Location]
fetchReferences store entry pos = do
    defs <- (fromMaybe [] <$>) . runMaybeT $ do
        ast <- liftMaybe entry.moduleAST
        references store ast pos
    infoM $ "Found " <> T.pack (show (length defs)) <> " reference(s)"
    return defs

references :: MonadIO m => I.IndexStore -> ModuleAST -> J.Position -> MaybeT m [J.Location]
references store ast pos = do
    -- Look up identifier under cursor
    (symbols, _) <- liftMaybe $ resolveAtPos store ast pos
    sequence $
        [ currySpanInfo2Location spi
        | s <- symbols
        , (_, mse) <- M.toList store.modules
        , ast' <- maybeToList mse.moduleAST
        , (qid, spi) <- (withSpanInfo <$> qualIdentifiers ast')
                     ++ (joinFst $ (maybeToList . qualIdentifier) <.$> withSpanInfo <$> expressions ast')
        , s' <- resolveQualIdent store ast' qid
        , s.kind == s'.kind && s.qualIdent == s'.qualIdent
        ]
    where withSpanInfo x = (x, CSPI.getSpanInfo x)
