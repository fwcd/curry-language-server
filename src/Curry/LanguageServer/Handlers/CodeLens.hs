{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Curry.LanguageServer.Handlers.CodeLens (codeLensHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Conversions (currySpanInfo2Range, currySpanInfo2Uri, ppToText)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

codeLensHandler :: S.Handlers LSM
codeLensHandler = S.requestHandler J.STextDocumentCodeLens $ \req responder -> do
    liftIO $ debugM "cls.codeLens" "Processing code lens request"
    let J.CodeLensParams _ _ doc = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    lenses <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchCodeLenses entry
    responder $ Right $ J.List $ fromMaybe [] lenses

fetchCodeLenses :: I.ModuleStoreEntry -> IO [J.CodeLens]
fetchCodeLenses entry = do
    lenses <- maybe (pure []) codeLenses $ I.mseModuleAST entry
    debugM "cls.codeLens" $ "Found " ++ show (length lenses) ++ " code lenses"
    return lenses

class HasCodeLenses s where
    codeLenses :: s -> IO [J.CodeLens]

instance HasCodeLenses (CS.Module CT.PredType) where
    codeLenses (CS.Module spi _ _ _ _ _ decls) = do
        maybeUri <- liftIO $ runMaybeT (currySpanInfo2Uri spi)

        let typeSigIdents = S.fromList [i | CS.TypeSig _ is _ <- decls, i <- is]
            untypedDecls = [(spi', i, t) | CS.FunctionDecl spi' t i _ <- decls, i `S.notMember` typeSigIdents]
            typeHintLenses = do
                (spi', i, t) <- untypedDecls
                range <- maybeToList $ currySpanInfo2Range spi'
                uri <- maybeToList maybeUri
                -- TODO: Move the command identifier ('decl.applyTypeHint') to some
                --       central place to avoid repetition.
                let text = ppToText i <> " :: " <> ppToText t
                    args = [A.toJSON uri, A.toJSON $ range ^. J.start, A.toJSON text]
                    command = J.Command text "decl.applyTypeHint" $ Just $ J.List args
                    lens = J.CodeLens range (Just command) Nothing
                return lens

        return typeHintLenses
