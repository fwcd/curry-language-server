module Curry.LanguageServer.Features.DocumentSymbols (fetchDocumentSymbols) where

import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Utils.Conversions
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchDocumentSymbols :: CompilationResult a -> IO J.DSResult
fetchDocumentSymbols compilation = do
    let symbols = maybe [] documentSymbols $ moduleAST <$> compilationToMaybe compilation
    U.logs $ "Found document symbols " ++ show symbols
    return $ J.DSDocumentSymbols $ J.List symbols