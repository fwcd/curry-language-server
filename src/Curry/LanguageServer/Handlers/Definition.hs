{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Definition (definitionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.TopEnv as CT

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
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
    responder $ Right $ J.InR $ J.InR $ J.List $ fromMaybe [] defs

fetchDefinitions :: I.IndexStore -> I.ModuleStoreEntry -> J.Position -> IO [J.LocationLink]
fetchDefinitions store entry pos = do
    defs <- runMaybeT $ do ast <- liftMaybe $ I.mseModuleAST entry
                           env <- liftMaybe $ I.mseCompilerEnv entry
                           MaybeT $ runLM (definition store pos) env ast
    infoM "cls.definition" $ "Found " ++ show defs
    return $ maybeToList defs

definition :: I.IndexStore -> J.Position -> LM J.LocationLink
definition store pos = do
    (qident, _) <- liftMaybe =<< findQualIdentAtPos pos
    Just (J.Location destUri destRange) <- (definitionInStore store qident <|>) <$> definitionInEnvs qident
    srcRange <- ((^. J.range) <$>) <$> (liftIO $ runMaybeT $ currySpanInfo2Location qident)
    return $ J.LocationLink srcRange destUri destRange destRange

definitionInStore :: I.IndexStore -> CI.QualIdent -> Maybe J.Location
definitionInStore store qident = find (isCurrySource . (^. J.uri)) locations
    where locations = (^. J.location) . I.sseSymbol <$> I.storedSymbolsByQualIdent qident store
          isCurrySource uri = ".curry" `T.isSuffixOf` J.getUri uri

definitionInEnvs :: CI.QualIdent -> LM (Maybe J.Location)
definitionInEnvs qident = do
    valueQIdent <- (CT.origName <$>) <$> lookupValueInfo qident
    typeQIdent  <- (CT.origName <$>) <$> lookupTypeInfo qident
    let origIdent = CI.qidIdent $ fromMaybe qident (valueQIdent <|> typeQIdent)
    liftIO $ runMaybeT $ currySpanInfo2Location (CSPI.getSpanInfo origIdent)
