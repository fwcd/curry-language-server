module Curry.LanguageServer.Features.WorkspaceSymbols (fetchWorkspaceSymbols) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

fetchWorkspaceSymbols :: T.Text -> [CS.Interface] -> IO [J.SymbolInformation]
fetchWorkspaceSymbols query interfaces = do
    logs INFO $ "fetchWorkspaceSymbols: Searching " ++ show (length interfaces) ++ " source file(s)..."
    let symbols = workspaceSymbols =<< interfaces
    logs INFO $ "fetchWorkspaceSymbols: Found " ++ show (length symbols) ++ " symbol(s)"
    return symbols
    
