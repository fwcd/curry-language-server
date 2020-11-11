module Curry.LanguageServer.Features.Diagnostics (fetchDiagnostics) where

import Control.Monad
import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Uri (normalizedUriToFilePath)
import qualified Language.Haskell.LSP.Types as J
import System.FilePath (takeBaseName)

fetchDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> IO [J.Diagnostic]
fetchDiagnostics uri entry = do
    let warnings = map (curryMsg2Diagnostic J.DsWarning) $ warningMessages entry
        errors = map (curryMsg2Diagnostic J.DsError) $ errorMessages entry
        diags = warnings ++ errors
        name = maybe "?" takeBaseName $ normalizedUriToFilePath uri
    
    unless (null diags) $
        logs INFO $ "fetchDiagnostics: Found " ++ show (length diags) ++ " message(s) in " ++ name

    return diags
