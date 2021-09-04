module Curry.LanguageServer.Handlers.SemanticTokens (semanticTokensHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

semanticTokensHandler :: S.Handlers LSM
semanticTokensHandler = S.requestHandler J.STextDocumentSemanticTokensFull $ \req responder -> do
    liftIO $ debugM "cls.semanticTokens" "Processing semantic tokens request"
    let doc = req ^. J.params . J.textDocument
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    responder $ Right $ Just $ J.SemanticTokens Nothing $ J.List []
