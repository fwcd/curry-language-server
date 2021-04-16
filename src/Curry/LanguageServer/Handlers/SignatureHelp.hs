{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.SignatureHelp (signatureHelpHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

signatureHelpHandler :: S.Handlers LSM
signatureHelpHandler = S.requestHandler J.STextDocumentSignatureHelp $ \req responder -> do
    liftIO $ debugM "cls.signatureHelp" "Processing signature help request"
    let J.SignatureHelpParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    sigHelp <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchSignatureHelp entry pos
    responder $ Right $ fromMaybe emptyHelp sigHelp
    where emptyHelp = J.SignatureHelp (J.List []) Nothing Nothing

fetchSignatureHelp :: I.ModuleStoreEntry -> J.Position -> IO J.SignatureHelp
fetchSignatureHelp entry pos = do
    sig <- fetchSignature entry pos
    let sigs = [sig]
        activeSig = 0
        activeParam = 0 -- TODO
    return $ J.SignatureHelp (J.List sigs) (Just activeSig) (Just activeParam)

fetchSignature :: I.ModuleStoreEntry -> J.Position -> IO J.SignatureInformation
fetchSignature entry pos = do
    -- TODO
    return $ J.SignatureInformation "Test" Nothing Nothing Nothing
