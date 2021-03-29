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
import Curry.LanguageServer.Utils.Lookup
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
definitions store ast@(CS.Module _ _ _ mid _ imps _) pos = do
    -- Find qualified identifier under cursor
    (qid, _) <- liftMaybe $ findQualIdentAtPos ast pos
    liftIO $ infoM "cls.definition" $ "Looking for " ++ ppToString qid
    -- Resolve the qualified identifier using imports
    -- TODO: Deal with aliases correctly
    let qids = qid : (flip CI.qualQualify qid <$>
               ([mid, CI.mkMIdent ["Prelude"]] ++ [mid' | CS.ImportDecl _ mid' _ _ _ <- imps]))
    -- Perform lookup
    let locs = definitionsInStore store =<< qids
    srcRange <- ((^. J.range) <$>) <$> (liftIO $ runMaybeT $ currySpanInfo2Location qid)
    return [J.LocationLink srcRange destUri destRange destRange | J.Location destUri destRange <- locs]

definitionsInStore :: I.IndexStore -> CI.QualIdent -> [J.Location]
definitionsInStore store qid = mapMaybe I.sLocation symbols'
    where symbols = I.storedSymbolsByQualIdent qid store
          symbols' | any I.sIsFromCurrySource symbols = filter I.sIsFromCurrySource symbols
                   | otherwise                        = symbols
