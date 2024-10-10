{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.TextDocument.Definition (definitionHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Index.Resolve (resolveAtPos)
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad (LSM, getStore)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Protocol.Message as J

definitionHandler :: S.Handlers LSM
definitionHandler = S.requestHandler J.SMethod_TextDocumentDefinition $ \req responder -> do
    debugM "Processing definition request"
    let pos = req ^. J.params . J.position
        uri = req ^. J.params . J.textDocument . J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    defs <- runMaybeT $ do
        lift $ debugM $ "Looking up " <> J.getUri (J.fromNormalizedUri normUri) <> " in " <> T.pack (show (M.keys store.modules))
        entry <- I.getModule normUri
        lift $ fetchDefinitions store entry pos
    responder $ Right $ J.InR $ maybe (J.InR J.Null) J.InL defs

fetchDefinitions :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> I.ModuleStoreEntry -> J.Position -> m [J.DefinitionLink]
fetchDefinitions store entry pos = do
    defs <- (fromMaybe [] <$>) $ runMaybeT $ do
        ast <- liftMaybe entry.moduleAST
        definitions store ast pos
    infoM $ "Found " <> T.pack (show (length defs)) <> " definition(s)"
    return defs

definitions :: MonadIO m => I.IndexStore -> ModuleAST -> J.Position -> MaybeT m [J.DefinitionLink]
definitions store ast pos = do
    -- Look up identifier under cursor
    (symbols, srcRange) <- liftMaybe $ resolveAtPos store ast pos
    let locations = mapMaybe (.location) symbols
    return [J.DefinitionLink $ J.LocationLink (Just srcRange) destUri destRange destRange | J.Location destUri destRange <- locations]
