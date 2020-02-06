module Curry.LanguageServer.Features.DocumentSymbols (fetchDocumentSymbols) where

import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchDocumentSymbols :: CompilationResult -> IO J.DSResult
fetchDocumentSymbols compilation = do
    let symbols = maybe [] documentSymbols $ moduleAST <$> compilationToMaybe compilation
    logs DEBUG $ "Found document symbols " ++ show symbols
    return $ J.DSDocumentSymbols $ J.List symbols
