module Curry.LanguageServer.Features.Diagnostics (fetchDiagnostics) where

import Control.Monad
import Control.Monad.Reader
import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Language.Haskell.LSP.Types as J

fetchDiagnostics :: CompilationResult a -> IO [J.Diagnostic]
fetchDiagnostics compilation = do
    let diags = case compilation of
                    Left errs -> map (curryMsg2Diagnostic J.DsError) errs
                    Right (_, warns) -> map (curryMsg2Diagnostic J.DsWarning) warns
    logs INFO $ "fetchDiagnostics: Found " ++ show diags
    return diags
