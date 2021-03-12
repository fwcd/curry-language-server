module Curry.LanguageServer.Features.DocumentSymbols (fetchDocumentSymbols) where

import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Utils.Conversions
import qualified Language.LSP.Types as J
import System.Log.Logger

fetchDocumentSymbols :: ModuleStoreEntry -> IO J.DSResult
fetchDocumentSymbols entry = do
    let symbols = maybe [] documentSymbols $ moduleAST entry
    debugM "cls.fetchDocumentSymbols" $ "Found document symbols " ++ show symbols
    return $ J.DSDocumentSymbols $ J.List symbols
