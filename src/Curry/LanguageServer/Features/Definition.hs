module Curry.LanguageServer.Features.Definition (fetchDefinitions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.TopEnv as CT

import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.IndexStore
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Data.Maybe (maybeToList, listToMaybe)
import qualified Language.Haskell.LSP.Types as J

fetchDefinitions :: IndexStore -> ModuleStoreEntry -> J.Position -> IO [J.Location]
fetchDefinitions store entry pos = do
    defs <- runMaybeT $ do ast <- liftMaybe $ moduleAST entry
                           env <- liftMaybe $ compilerEnv entry
                           MaybeT $ runLM (definition store pos) env ast
    logs INFO $ "fetchDefinitions: Found " ++ show defs
    return $ maybeToList defs

definition :: IndexStore -> J.Position -> LM J.Location
definition store pos = do
    (qident, spi) <- qualIdentAtPos pos
    qident' <- lift $ runMaybeT $ CT.origName <$> lookupValueInfo qident
    let ident  = CI.qidIdent  $  qident
        ident' = CI.qidIdent <$> qident'
    liftMaybe =<< (liftIO $ runMaybeT $ currySpanInfo2Location $ CSPI.getSpanInfo $ maybe ident id ident')
