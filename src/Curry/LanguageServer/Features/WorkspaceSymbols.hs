module Curry.LanguageServer.Features.WorkspaceSymbols (fetchWorkspaceSymbols) where

import Curry.LanguageServer.IndexStore
import Curry.LanguageServer.Logging
import qualified Data.Text as T
import qualified Language.LSP.Types as J

fetchWorkspaceSymbols :: IndexStore -> T.Text -> IO [J.SymbolInformation]
fetchWorkspaceSymbols store query = do
    logs DEBUG $ "fetchWorkspaceSymbols: Searching " ++ show (storedSymbolCount store) ++ " symbol(s)..."
    let symbols = symbol <$> storedSymbolsWithPrefix query store
    logs INFO $ "fetchWorkspaceSymbols: Found " ++ show (length symbols) ++ " symbol(s)"
    return symbols

matchesQuery :: T.Text -> J.SymbolInformation -> Bool
matchesQuery query (J.SymbolInformation n _ _ _ _) = query `T.isPrefixOf` n
