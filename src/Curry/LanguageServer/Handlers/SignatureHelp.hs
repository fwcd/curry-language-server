{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.SignatureHelp (signatureHelpHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Curry.LanguageServer.Index.Resolve (resolveQualIdent)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.General (liftMaybe, lastSafe, snapToLastTokenEnd)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Syntax
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import qualified Language.LSP.VFS as VFS
import System.Log.Logger

import Curry.LanguageServer.Utils.Convert (ppToString)
import Debug.Trace

signatureHelpHandler :: S.Handlers LSM
signatureHelpHandler = S.requestHandler J.STextDocumentSignatureHelp $ \req responder -> do
    liftIO $ debugM "cls.signatureHelp" "Processing signature help request"
    let J.SignatureHelpParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    sigHelp <- runMaybeT $ do
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri
        liftMaybe =<< (liftIO $ fetchSignatureHelp store entry vfile pos)
    responder $ Right $ fromMaybe emptyHelp sigHelp
    where emptyHelp = J.SignatureHelp (J.List []) Nothing Nothing

fetchSignatureHelp :: I.IndexStore -> I.ModuleStoreEntry -> VFS.VirtualFile -> J.Position -> IO (Maybe J.SignatureHelp)
fetchSignatureHelp store entry vfile pos@(J.Position l c) = runMaybeT $ do
    ast <- liftMaybe $ I.mseModuleAST entry
    let line = VFS.rangeLinesFromVfs vfile $ J.Range (J.Position l 0) (J.Position (l + 1) 0)
        c'   = snapToLastTokenEnd (T.unpack line) c
        pos' = J.Position l c'
    (sym, args) <- liftMaybe $  findExpressionApplication store ast pos'
                            <|> findTypeApplication       store ast pos'
    liftIO $ infoM "cls.signatureHelp" $ "Found symbol " ++ T.unpack (I.sQualIdent sym)
    let activeParam = maybe (length args) fst $ find (elementContains pos . snd) (zip [0..] args)
        activeSig = 0
        labelStart = I.sQualIdent sym <> " :: "
        paramSep = " -> "
        paramLabels = I.sPrintedArgumentTypes sym
        paramOffsets = reverse $ snd $ foldl (\(n, offs) lbl -> let n' = n + T.length lbl in (n' + T.length paramSep, (n, n') : offs)) (T.length labelStart, []) paramLabels
        params = flip J.ParameterInformation Nothing . uncurry J.ParameterLabelOffset <$> paramOffsets
        label = labelStart <> T.intercalate paramSep (paramLabels ++ maybeToList (I.sPrintedResultType sym))
        sig = J.SignatureInformation label Nothing (Just $ J.List params) (Just activeParam)
        sigs = [sig]
    return $ J.SignatureHelp (J.List sigs) (Just activeSig) (Just activeParam)

findExpressionApplication :: I.IndexStore -> ModuleAST -> J.Position -> Maybe (I.Symbol, [CSPI.SpanInfo])
findExpressionApplication store ast pos = lastSafe $ do
    let exprs = elementsAt pos $ expressions ast
    e@(CS.Apply _ _ _) <- exprs
    let base : args = appFull e
    sym <- maybeToList $ lookupBaseExpression store ast base
    return (sym, CSPI.getSpanInfo <$> args)

findTypeApplication :: I.IndexStore -> ModuleAST -> J.Position -> Maybe (I.Symbol, [CSPI.SpanInfo])
findTypeApplication store ast pos = lastSafe $ do
    let ts = elementsAt pos $ typeExpressions ast
    traceM $ "Found these: " ++ show (ppToString <$> ts)
    e@(CS.ApplyType _ _ _) <- ts
    traceM $ "Found apply type " ++ ppToString e
    let base : args = typeAppFull e
    traceM $ "Found base " ++ show base
    sym <- maybeToList $ lookupBaseTypeExpression store ast base
    traceM $ "Found symbol " ++ show sym
    return (sym, CSPI.getSpanInfo <$> args)

lookupBaseTypeExpression :: I.IndexStore -> ModuleAST -> CS.TypeExpr -> Maybe I.Symbol
lookupBaseTypeExpression store ast e = listToMaybe $ case e of
    CS.ConstructorType _ q -> resolveQualIdent store ast q
    CS.VariableType _ i    -> resolveQualIdent store ast $ CI.qualify i
    _                      -> []

lookupBaseExpression :: I.IndexStore -> ModuleAST -> CS.Expression a -> Maybe I.Symbol
lookupBaseExpression store ast e = listToMaybe $ case e of
    CS.Variable _ _ q    -> resolveQualIdent store ast q
    CS.Constructor _ _ q -> resolveQualIdent store ast q
    _                    -> []
