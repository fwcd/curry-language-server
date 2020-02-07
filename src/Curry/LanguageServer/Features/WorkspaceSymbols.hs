module Curry.LanguageServer.Features.WorkspaceSymbols (fetchWorkspaceSymbols) where

import Curry.LanguageServer.IndexStore
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

fetchWorkspaceSymbols :: T.Text -> IndexStore -> IO [J.SymbolInformation]
fetchWorkspaceSymbols query store = do
    logs DEBUG $ "fetchWorkspaceSymbols: Searching " ++ show (storedCount store) ++ " source file(s)..."
    let asts = (maybeToList . moduleAST . snd) =<< storedEntries store
        symbols = filter (matchesQuery query) $ workspaceSymbols =<< asts
    logs INFO $ "fetchWorkspaceSymbols: Found " ++ show (length symbols) ++ " symbol(s)"
    return symbols

matchesQuery :: T.Text -> J.SymbolInformation -> Bool
matchesQuery query (J.SymbolInformation n _ _ _ _) = query `T.isPrefixOf` n
