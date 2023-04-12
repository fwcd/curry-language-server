module Curry.LanguageServer.Handlers.TextDocument.DocumentSymbol (documentSymbolHandler) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Lens ((^.))
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Utils.Convert (HasDocumentSymbols(..))
import Curry.LanguageServer.Monad (LSM)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

documentSymbolHandler :: S.Handlers LSM
documentSymbolHandler = S.requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
    liftIO $ debugM "cls.documentSymbols" "Processing document symbols request"
    let uri = req ^. J.params . J.textDocument . J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    symbols <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchDocumentSymbols entry
    responder $ Right $ J.InL $ J.List $ fromMaybe [] symbols

fetchDocumentSymbols :: I.ModuleStoreEntry -> IO [J.DocumentSymbol]
fetchDocumentSymbols entry = do
    let symbols = maybe [] documentSymbols $ I.mseModuleAST entry
    debugM "cls.documentSymbols" $ "Found document symbols " ++ show symbols
    return symbols
