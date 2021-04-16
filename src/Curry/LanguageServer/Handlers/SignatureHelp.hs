{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.SignatureHelp (signatureHelpHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Curry.LanguageServer.Index.Resolve (resolveQualIdent)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.General (liftMaybe, lastSafe)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Syntax
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

import Debug.Trace
import Curry.LanguageServer.Utils.Convert (ppToString)

signatureHelpHandler :: S.Handlers LSM
signatureHelpHandler = S.requestHandler J.STextDocumentSignatureHelp $ \req responder -> do
    liftIO $ debugM "cls.signatureHelp" "Processing signature help request"
    let J.SignatureHelpParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    sigHelp <- runMaybeT $ do
        entry <- I.getModule normUri
        liftMaybe =<< (liftIO $ fetchSignatureHelp store entry pos)
    responder $ Right $ fromMaybe emptyHelp sigHelp
    where emptyHelp = J.SignatureHelp (J.List []) Nothing Nothing

fetchSignatureHelp :: I.IndexStore -> I.ModuleStoreEntry -> J.Position -> IO (Maybe J.SignatureHelp)
fetchSignatureHelp store entry pos = runMaybeT $ do
    ast <- liftMaybe $ I.mseModuleAST entry
    let exprs = elementsAt pos $ expressions ast
    -- TODO: Type applications?
    (sym, args) <- liftMaybe $ lastSafe $ do
        e@(CS.Apply _ _ _) <- exprs
        let base : args = appFull e
        traceM $ "Signature help expr: " ++ ppToString e ++ ", base: " ++ ppToString base ++ ", args: " ++ show (ppToString <$> args)
        traceM $ "Base is " ++ show base
        sym <- maybeToList $ lookupExpression store ast base
        traceM $ "Found symbol " ++ show sym
        return (sym, args)
    let activeParam = maybe 0 fst $ find (elementContains pos . snd) (zip [0..] args)
        activeSig = 0
        paramLabels = I.sPrintedArgumentTypes sym
        params = flip J.ParameterInformation Nothing . J.ParameterLabelString <$> paramLabels
        label = T.intercalate " -> " paramLabels
        sig = J.SignatureInformation label Nothing (Just $ J.List params) (Just activeParam)
        sigs = [sig]
    return $ J.SignatureHelp (J.List sigs) (Just activeSig) (Just activeParam)

lookupExpression :: I.IndexStore -> ModuleAST -> CS.Expression a -> Maybe I.Symbol
lookupExpression store ast e = listToMaybe $ case e of
    CS.Variable _ _ q    -> resolveQualIdent store ast q
    CS.Constructor _ _ q -> resolveQualIdent store ast q
    _                    -> []
