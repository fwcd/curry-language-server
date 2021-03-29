{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Curry.LanguageServer.Handlers.Hover (hoverHandler) where

-- Curry Compiler Libraries + Dependencies

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (ppPredTypeToText, currySpanInfo2Range)
import Curry.LanguageServer.Index.Lookup
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Syntax (ModuleAST, moduleIdentifier)
import Curry.LanguageServer.Utils.Sema (TypedSpanInfo (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

hoverHandler :: S.Handlers LSM
hoverHandler = S.requestHandler J.STextDocumentHover $ \req responder -> do
    liftIO $ debugM "cls.hover" "Processing hover request"
    -- TODO: Update once https://github.com/haskell/lsp/issues/303 is fixed
    let J.HoverParams doc pos _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    hover <- runMaybeT $ do
        entry <- I.getModule normUri
        liftMaybe =<< liftIO (fetchHover store entry pos)
    responder $ Right hover

fetchHover :: I.IndexStore -> I.ModuleStoreEntry -> J.Position -> IO (Maybe J.Hover)
fetchHover store entry pos = runMaybeT $ do
    ast <- liftMaybe $ I.mseModuleAST entry
    hover <- liftMaybe $ qualIdentHover store ast pos <|> typedSpanInfoHover ast pos
    liftIO $ infoM "cls.hover" $ "Found hover: " ++ T.unpack (previewHover hover)
    return hover

qualIdentHover :: I.IndexStore -> ModuleAST -> J.Position -> Maybe J.Hover
qualIdentHover store ast pos = do
    (symbols, range) <- resolveQualIdentAtPos store ast pos
    s <- listToMaybe symbols

    let contents = J.HoverContents $ J.markedUpContent "curry" $ I.sQualIdent s <> maybe "" (" :: " <>) (I.sPrintedType s)

    return $ J.Hover contents $ Just range

typedSpanInfoHover :: ModuleAST -> J.Position -> Maybe J.Hover
typedSpanInfoHover ast@(moduleIdentifier -> mid) pos = do
    TypedSpanInfo txt t spi <- findTypeAtPos ast pos

    let contents = J.HoverContents $ J.markedUpContent "curry" $ txt <> " :: " <> ppPredTypeToText mid t
        range = currySpanInfo2Range spi

    return $ J.Hover contents range

previewHover :: J.Hover -> T.Text
previewHover ((^. J.contents) -> J.HoverContents (J.MarkupContent k t)) = case k of J.MkMarkdown  -> markdownToPlain t
                                                                                    J.MkPlainText -> t
previewHover _                                                          = "?"

markdownToPlain :: T.Text -> T.Text
markdownToPlain t = T.intercalate ", " $ filter includeLine $ T.lines t
    where includeLine l = not ("```" `T.isPrefixOf` l || T.null l)
