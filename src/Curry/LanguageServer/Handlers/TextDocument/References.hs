{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.TextDocument.References (referencesHandler) where

import Curry.LanguageServer.Monad (LSM)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Types as J
import Curry.LanguageServer.Utils.Logging (debugM)

referencesHandler :: S.Handlers LSM
referencesHandler = S.requestHandler J.SMethod_TextDocumentReferences $ \req responder -> do
    debugM "Processing references request"
    responder $ Right $ J.InL []
