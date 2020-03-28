module Curry.LanguageServer.Features.DocumentSymbols (fetchDocumentSymbols) where

import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchDocumentSymbols :: ModuleStoreEntry -> IO J.DSResult
fetchDocumentSymbols entry = do
    let symbols = maybe [] documentSymbols $ moduleAST entry
    logs DEBUG $ "Found document symbols " ++ show symbols
    return $ J.DSDocumentSymbols $ J.List symbols
