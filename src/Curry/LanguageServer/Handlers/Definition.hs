{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Definition (definitionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Syntax as CS

import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Index.Lookup
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
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
    defs <- (fromMaybe [] <$>) $ runMaybeT $ do
        ast <- liftMaybe $ I.mseModuleAST entry
        definitions store ast pos
    infoM "cls.definition" $ "Found " ++ show defs
    return defs

definitions :: I.IndexStore -> ModuleAST -> J.Position -> MaybeT IO [J.LocationLink]
definitions store ast pos = do
    -- Look up qualified identifier under cursor
    (symbols, srcRange) <- liftMaybe $ resolveQualIdentAtPos store ast pos
    let locations = mapMaybe I.sLocation symbols
    return [J.LocationLink (Just srcRange) destUri destRange destRange | J.Location destUri destRange <- locations]
