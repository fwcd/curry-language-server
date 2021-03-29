{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Curry.LanguageServer.Handlers.CodeAction (codeActionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Uri, currySpanInfo2Range, ppToText)
import Curry.LanguageServer.Utils.General (rangeOverlaps)
import Curry.LanguageServer.Utils.Sema (untypedTopLevelDecls)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe, maybeToList)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

codeActionHandler :: S.Handlers LSM
codeActionHandler = S.requestHandler J.STextDocumentCodeAction $ \req responder -> do
    liftIO $ debugM "cls.codeAction" "Processing code action request"
    let J.CodeActionParams _ _ doc range _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    actions <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchCodeActions range entry
    responder $ Right $ J.List $ J.InR <$> fromMaybe [] actions

fetchCodeActions :: J.Range -> I.ModuleStoreEntry -> IO [J.CodeAction]
fetchCodeActions range entry = do
    actions <- maybe (pure []) (codeActions range) $ I.mseModuleAST entry
    debugM "cls.codeAction" $ "Found " ++ show (length actions) ++ " code action(s)"
    return actions

class HasCodeActions s where
    codeActions :: J.Range -> s -> IO [J.CodeAction]

instance HasCodeActions (CS.Module (Maybe CT.PredType)) where
    codeActions range mdl@(CS.Module spi _ _ _ _ _ _) = do
        maybeUri <- liftIO $ runMaybeT (currySpanInfo2Uri spi)

        -- TODO: Attach diagnostics, ideally the frontend could emit these
        --       quick fixes along with the warning messages?

        let typeHintActions = do
                (spi', i, tp) <- untypedTopLevelDecls mdl
                t <- maybeToList tp
                range' <- maybeToList $ currySpanInfo2Range spi'
                guard $ rangeOverlaps range range'
                uri <- maybeToList maybeUri
                -- TODO: Move the command identifier ('decl.applyTypeHint') to some
                --       central place to avoid repetition.
                let text = ppToText i <> " :: " <> ppToText t
                    args = [A.toJSON uri, A.toJSON $ range' ^. J.start, A.toJSON text]
                    command = J.Command text "decl.applyTypeHint" $ Just $ J.List args
                    caKind = J.CodeActionQuickFix
                    isPreferred = True
                    lens = J.CodeAction ("Add type annotation '" <> text <> "'") (Just caKind) Nothing (Just isPreferred) Nothing $ Just command
                return lens

        return typeHintActions

