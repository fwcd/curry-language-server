{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.SignatureHelp (signatureHelpHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Convert (ppToText)
import Curry.LanguageServer.Utils.General (liftMaybe, lastSafe)
import Curry.LanguageServer.Utils.Syntax
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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
        liftMaybe =<< (liftIO $ fetchSignatureHelp entry pos)
    responder $ Right $ fromMaybe emptyHelp sigHelp
    where emptyHelp = J.SignatureHelp (J.List []) Nothing Nothing

fetchSignatureHelp :: I.ModuleStoreEntry -> J.Position -> IO (Maybe J.SignatureHelp)
fetchSignatureHelp entry pos = runMaybeT $ do
    let ast = I.mseModuleAST entry
        exprs = elementsAt pos $ expressions ast
    -- TODO: Type applications?
    -- TODO: Extract types rather than printing the values
    (e1, e2) <- liftMaybe $ lastSafe [(e1, e2) | CS.Apply _ e1 e2 <- exprs]
    let activeSig = 0
        activeParam | elementContains pos e1 = 0
                    | otherwise              = 1
        paramLabels = ppToText <$> [e1, e2]
        params = flip J.ParameterInformation Nothing . J.ParameterLabelString <$> paramLabels
        label = T.intercalate " -> " paramLabels
        sig = J.SignatureInformation label Nothing (Just $ J.List params) (Just activeParam)
        sigs = [sig]
    return $ J.SignatureHelp (J.List sigs) (Just activeSig) (Just activeParam)
