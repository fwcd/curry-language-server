{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Hover (hoverHandler) where

import Control.Lens ((^.))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Utils.Lookup
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Syntax (TypedSpanInfo (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
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
    hover <- runMaybeT $ do
        entry <- I.getModule normUri
        liftMaybe =<< liftIO (fetchHover entry pos)
    responder $ Right hover

fetchHover :: I.ModuleStoreEntry -> J.Position -> IO (Maybe J.Hover)
fetchHover entry pos = runMaybeT $ do
    ast <- liftMaybe $ I.mseModuleAST entry
    hover <- MaybeT $ runLM (hoverAt pos) ast
    liftIO $ infoM "cls.hover" $ "Found " ++ show hover
    return hover

hoverAt :: J.Position -> LM J.Hover
hoverAt pos = typedSpanInfoHover pos >>= liftMaybe

typedSpanInfoHover :: J.Position -> LM (Maybe J.Hover)
typedSpanInfoHover pos = runMaybeT $ do
    TypedSpanInfo txt t spi <- MaybeT $ findTypeAtPos pos
    mid <- lift getModuleIdentifier

    let contents = J.HoverContents $ J.markedUpContent "curry" $ txt <> " :: " <> ppPredTypeToText mid t
        range = currySpanInfo2Range spi

    return $ J.Hover contents range
