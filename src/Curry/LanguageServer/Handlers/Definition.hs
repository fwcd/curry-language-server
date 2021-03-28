{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Definition (definitionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI

import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Utils.Lookup
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList, mapMaybe)
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
                           env <- liftMaybe Nothing -- FIXME
                           MaybeT $ runLM (definition store pos) env ast
    infoM "cls.definition" $ "Found " ++ show defs
    return $ maybeToList defs

definition :: I.IndexStore -> J.Position -> LM J.LocationLink
definition store pos = do
    (qident, _) <- liftMaybe =<< findQualIdentAtPos pos
    J.Location destUri destRange <- liftMaybe $ definitionInStore store qident
    srcRange <- ((^. J.range) <$>) <$> (liftIO $ runMaybeT $ currySpanInfo2Location qident)
    return $ J.LocationLink srcRange destUri destRange destRange

definitionInStore :: I.IndexStore -> CI.QualIdent -> Maybe J.Location
definitionInStore store qident = find (isCurrySource . (^. J.uri)) locations
    where locations = mapMaybe I.sLocation $ I.storedSymbolsByQualIdent qident store
          isCurrySource uri = ".curry" `T.isSuffixOf` J.getUri uri
