module Curry.LanguageServer.Handlers.DocumentSymbols (fetchDocumentSymbols) where

import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Utils.Conversions
import qualified Language.LSP.Types as J

fetchDocumentSymbols :: ModuleStoreEntry -> IO J.DSResult
fetchDocumentSymbols entry = do
    let symbols = maybe [] documentSymbols $ moduleAST entry
    logs DEBUG $ "Found document symbols " ++ show symbols
    return $ J.DSDocumentSymbols $ J.List symbols
