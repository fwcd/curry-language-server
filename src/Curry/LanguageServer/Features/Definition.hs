module Curry.LanguageServer.Features.Definition (fetchDefinitions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.SpanInfo as CSPI

import Control.Monad.Trans (liftIO)
import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General
import Data.Maybe (maybeToList)
import qualified Language.Haskell.LSP.Types as J

fetchDefinitions :: IndexStoreEntry -> J.Position -> IO [J.Location]
fetchDefinitions entry pos = do
    let defs = maybeToList $ do
                    ast <- moduleAST entry
                    env <- compilerEnv entry
                    runLM (definitionAt pos) env ast
    liftIO $ logs INFO $ "fetchDefinitions: Found " ++ show defs
    return defs

definitionAt :: J.Position -> LM J.Location
definitionAt pos = do
    (ident, spi) <- findAtPos pos
    liftMaybe $ currySpanInfo2Location $ CSPI.getSpanInfo ident
