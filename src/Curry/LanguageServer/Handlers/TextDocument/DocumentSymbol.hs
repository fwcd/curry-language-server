{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.DocumentSymbol (documentSymbolHandler) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Lens ((^.))
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Utils.Logging (debugM)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Utils.Convert (HasDocumentSymbols(..))
import Curry.LanguageServer.Monad (LSM)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Protocol.Message as J

documentSymbolHandler :: S.Handlers LSM
documentSymbolHandler = S.requestHandler J.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
    debugM "Processing document symbols request"
    let uri = req ^. J.params . J.textDocument . J.uri
    normUri <- normalizeUriWithPath uri
    symbols <- runMaybeT $ do
        entry <- I.getModule normUri
        lift $ fetchDocumentSymbols entry
    responder $ Right $ J.InR $ maybe (J.InR J.Null) J.InL symbols

fetchDocumentSymbols :: (MonadIO m, MonadLsp CFG.Config m) => I.ModuleStoreEntry -> m [J.DocumentSymbol]
fetchDocumentSymbols entry = do
    let symbols = maybe [] documentSymbols entry.moduleAST
    debugM $ "Found document symbols " <> T.pack (show symbols)
    return symbols
