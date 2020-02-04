module Curry.LanguageServer.Features.Hover (fetchHover) where

import Curry.LanguageServer.Compiler
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchHover :: CompilationResult -> J.Position -> IO (Maybe J.Hover)
fetchHover compilation pos = do
    -- TODO
    return Nothing
    -- let hover =  <$> ast <$> compilation
    -- U.logs $ "fetchHover: Found " ++ show hover
    -- return hover

