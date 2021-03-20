module Curry.LanguageServer.Handlers.Definition (definitionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.TopEnv as CT

import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

definitionHandler :: S.Handlers LSM
definitionHandler = S.requestHandler J.STextDocumentDefinition $ \req responder -> do
    liftIO $ debugM "cls.definition" "Processing definition request"
    -- TODO: Update once https://github.com/haskell/lsp/issues/303 is fixed
    let J.DefinitionParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    defs <- runMaybeT $ do
        liftIO $ debugM "cls.definition" $ "Looking up " ++ show normUri ++ " in " ++ show (M.keys $ I.idxModules store)
        entry <- I.getModule normUri
        liftIO $ fetchDefinitions store entry pos
    responder $ Right $ J.InR $ J.InL $ J.List $ fromMaybe [] defs

fetchDefinitions :: I.IndexStore -> I.ModuleStoreEntry -> J.Position -> IO [J.Location]
fetchDefinitions store entry pos = do
    defs <- runMaybeT $ do ast <- liftMaybe $ I.mseModuleAST entry
                           env <- liftMaybe $ I.mseCompilerEnv entry
                           MaybeT $ runLM (definition store pos) env ast
    infoM "cls.definition" $ "Found " ++ show defs
    return $ maybeToList defs

definition :: I.IndexStore -> J.Position -> LM J.Location
definition _ pos = do
    (qident, _) <- liftMaybe =<< findQualIdentAtPos pos
    valueInfo   <- lookupValueInfo qident
    let qident' = CT.origName <$> valueInfo
        ident   = CI.qidIdent qident
        ident'  = CI.qidIdent <$> qident'
    liftMaybe =<< (liftIO $ runMaybeT $ currySpanInfo2Location $ CSPI.getSpanInfo $ fromMaybe ident ident')
