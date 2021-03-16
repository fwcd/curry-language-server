{-# LANGUAGE FlexibleInstances #-}
module Curry.LanguageServer.Handlers.CodeAction (codeActionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

codeActionHandler :: S.Handlers LSM
codeActionHandler = S.requestHandler J.STextDocumentCodeAction $ \req responder -> do
    liftIO $ debugM "cls.codeAction" "Processing code action request"
    let uri = req ^. J.params . J.textDocument . J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    actions <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchCodeActions entry
    responder $ Right $ J.List $ J.InR <$> fromMaybe [] actions

fetchCodeActions :: I.ModuleStoreEntry -> IO [J.CodeAction]
fetchCodeActions entry = do
    actions <- maybe (pure []) codeActions $ I.mseModuleAST entry
    debugM "cls.codeAction" $ "Found " ++ show (length actions) ++ " code actions"
    return actions

class HasCodeActions s where
    codeActions :: s -> IO [J.CodeAction]

instance HasCodeActions (CS.Module CT.PredType) where
    codeActions = undefined -- TODO
