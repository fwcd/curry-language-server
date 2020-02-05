module Curry.LanguageServer.Features.WorkspaceSymbols (fetchWorkspaceSymbols) where

import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

fetchWorkspaceSymbols :: T.Text -> [CompilationResult a] -> IO [J.SymbolInformation]
fetchWorkspaceSymbols query compilations = do
    -- TODO: Handle query
    logs INFO $ "fetchWorkspaceSymbols: Searching " ++ show (length compilations) ++ " source file(s)..."
    let asts = moduleAST <$> ((maybeToList . compilationsToMaybe) =<< compilations)
        symbols = workspaceSymbolsWithUri uri =<< asts
    logs INFO $ "fetchWorkspaceSymbols: Found " ++ show (length symbols) ++ " symbol(s)"
    return symbols
    
