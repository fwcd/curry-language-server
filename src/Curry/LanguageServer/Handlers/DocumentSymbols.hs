module Curry.LanguageServer.Handlers.DocumentSymbols (documentSymbolHandler) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Lens ((^.))
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Uri (filePathToNormalizedUri, normalizeUriWithPath)
import Curry.LanguageServer.Utils.Conversions (HasDocumentSymbols(..))
import Curry.LanguageServer.Monad
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
    let symbols = maybe [] documentSymbols $ I.moduleAST entry
    debugM "cls.documentSymbols" $ "Found document symbols " ++ show symbols
    return symbols
