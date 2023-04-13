{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.TextDocument.Definition (definitionHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Index.Resolve (resolveQualIdentAtPos)
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad (LSM, getStore)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import Language.LSP.Server (MonadLsp)

definitionHandler :: S.Handlers LSM
definitionHandler = S.requestHandler J.STextDocumentDefinition $ \req responder -> do
    debugM "Processing definition request"
    -- TODO: Update once https://github.com/haskell/lsp/issues/303 is fixed
    let J.DefinitionParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    defs <- runMaybeT $ do
        lift $ debugM $ "Looking up " <> J.getUri (J.fromNormalizedUri normUri) <> " in " <> T.pack (show (M.keys $ I.idxModules store))
        entry <- I.getModule normUri
        lift $ fetchDefinitions store entry pos
    responder $ Right $ J.InR $ J.InR $ J.List $ fromMaybe [] defs

fetchDefinitions :: (MonadIO m, MonadLsp c m) => I.IndexStore -> I.ModuleStoreEntry -> J.Position -> m [J.LocationLink]
fetchDefinitions store entry pos = do
    defs <- (fromMaybe [] <$>) $ runMaybeT $ do
        ast <- liftMaybe $ I.mseModuleAST entry
        definitions store ast pos
    infoM $ "Found " <> T.pack (show (length defs)) <> " definition(s)"
    return defs

definitions :: MonadIO m => I.IndexStore -> ModuleAST -> J.Position -> MaybeT m [J.LocationLink]
definitions store ast pos = do
    -- Look up qualified identifier under cursor
    (symbols, srcRange) <- liftMaybe $ resolveQualIdentAtPos store ast pos
    let locations = mapMaybe I.sLocation symbols
    return [J.LocationLink (Just srcRange) destUri destRange destRange | J.Location destUri destRange <- locations]
