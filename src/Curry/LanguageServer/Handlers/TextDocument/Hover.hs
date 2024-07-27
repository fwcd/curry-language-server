{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedRecordDot, TypeOperators, ViewPatterns #-}
module Curry.LanguageServer.Handlers.TextDocument.Hover (hoverHandler) where

-- Curry Compiler Libraries + Dependencies

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (ppPredTypeToText, currySpanInfo2Range)
import Curry.LanguageServer.Index.Resolve (resolveAtPos)
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Lookup (findTypeAtPos)
import Curry.LanguageServer.Utils.Syntax (moduleIdentifier)
import Curry.LanguageServer.Utils.Sema (ModuleAST, TypedSpanInfo (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad (LSM, getStore)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (MonadLsp)

hoverHandler :: S.Handlers LSM
hoverHandler = S.requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
    debugM "Processing hover request"
    let pos = req ^. J.params . J.position
        uri = req ^. J.params . J.textDocument . J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    hover <- runMaybeT $ do
        entry <- I.getModule normUri
        MaybeT $ fetchHover store entry pos
    responder $ Right $ maybe (J.InR J.Null) J.InL hover

fetchHover :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> I.ModuleStoreEntry -> J.Position -> m (Maybe J.Hover)
fetchHover store entry pos = runMaybeT $ do
    ast <- liftMaybe entry.moduleAST
    hover <- liftMaybe $ qualIdentHover store ast pos <|> typedSpanInfoHover ast pos
    lift $ infoM $ "Found hover: " <> previewHover hover
    return hover

qualIdentHover :: I.IndexStore -> ModuleAST -> J.Position -> Maybe J.Hover
qualIdentHover store ast pos = do
    (symbols, range) <- resolveAtPos store ast pos
    s <- listToMaybe symbols

    let contents = J.InL $ J.mkMarkdownCodeBlock "curry" $ s.qualIdent <> maybe "" (" :: " <>) s.printedType

    return $ J.Hover contents $ Just range

typedSpanInfoHover :: ModuleAST -> J.Position -> Maybe J.Hover
typedSpanInfoHover ast@(moduleIdentifier -> mid) pos = do
    TypedSpanInfo txt t spi <- findTypeAtPos ast pos

    let contents = J.InL $ J.mkMarkdownCodeBlock "curry" $ txt <> " :: " <> maybe "?" (ppPredTypeToText mid) t
        range = currySpanInfo2Range spi

    return $ J.Hover contents range

previewHover :: J.Hover -> T.Text
previewHover ((^. J.contents) -> J.InL (J.MarkupContent k t)) = case k of J.MarkupKind_Markdown  -> markdownToPlain t
                                                                          J.MarkupKind_PlainText -> t
previewHover _                                                          = "?"

markdownToPlain :: T.Text -> T.Text
markdownToPlain t = T.intercalate ", " $ filter includeLine $ T.lines t
    where includeLine l = not ("```" `T.isPrefixOf` l || T.null l)
