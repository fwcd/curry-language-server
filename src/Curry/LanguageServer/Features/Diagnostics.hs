module Curry.LanguageServer.Features.Diagnostics (fetchDiagnostics) where

import Control.Monad
import Control.Monad.Reader
import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Language.Haskell.LSP.Types as J

fetchDiagnostics :: IndexStoreEntry -> IO [J.Diagnostic]
fetchDiagnostics entry = do
    let warnings = map (curryMsg2Diagnostic J.DsWarning) $ warningMessages entry
        errors = map (curryMsg2Diagnostic J.DsError) $ errorMessages entry
        diags = warnings ++ errors
    logs INFO $ "fetchDiagnostics: Found " ++ show (length diags) ++ " message(s)"
    return diags
