{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Hover (hoverHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Base.TopEnv as CT

import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

hoverHandler :: S.Handlers LSM
hoverHandler = S.requestHandler J.STextDocumentHover $ \req responder -> do
    liftIO $ debugM "cls.reactor" "Processing hover request"
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
    ast <- liftMaybe $ I.moduleAST entry
    env <- liftMaybe $ I.compilerEnv entry
    hover <- MaybeT $ runLM (hoverAt pos) env ast
    liftIO $ infoM "cls.hover" $ "Found " ++ show hover
    return hover

hoverAt :: J.Position -> LM J.Hover
hoverAt pos = do
    (ident, spi) <- findAtPos pos
    valueInfo <- lookupValueInfo ident
    let msg = J.HoverContents $ J.markedUpContent "curry" $ ppToText (CT.origName valueInfo) <> " :: " <> ppToText (valueInfoType valueInfo)
        range = currySpanInfo2Range spi
    return $ J.Hover msg range
