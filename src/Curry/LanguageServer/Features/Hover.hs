module Curry.LanguageServer.Features.Hover (fetchHover) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchHover :: CompilationResult a -> J.Position -> IO (Maybe J.Hover)
fetchHover compilation pos = do
    -- TODO
    return Nothing
    -- let hover = expressionAt pos <$> ast <$> compilation
    -- U.logs $ "fetchHover: Found " ++ show hover
    -- return hover
