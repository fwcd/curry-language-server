{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedRecordDot, MonadComprehensions #-}
module Curry.LanguageServer.Handlers.TextDocument.SignatureHelp (signatureHelpHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Index.Resolve (resolveQualIdent)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Monad (LSM, getStore)
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range)
import Curry.LanguageServer.Utils.General (liftMaybe, lastSafe, snapToLastTokenEnd)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Syntax
    ( appFull, elementContains, elementsAt
    , typeAppFull, HasExpressions (..)
    , HasTypeExpressions (..)
    )
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Utils.Logging (infoM, debugM)
import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.VFS as VFS
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Protocol.Message as J

signatureHelpHandler :: S.Handlers LSM
signatureHelpHandler = S.requestHandler J.SMethod_TextDocumentSignatureHelp $ \req responder -> do
    debugM "Processing signature help request"
    let J.SignatureHelpParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    sigHelp <- runMaybeT $ do
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri
        MaybeT $ fetchSignatureHelp store entry vfile pos
    responder $ Right $ maybe (J.InR J.Null) J.InL sigHelp

fetchSignatureHelp :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> I.ModuleStoreEntry -> VFS.VirtualFile -> J.Position -> m (Maybe J.SignatureHelp)
fetchSignatureHelp store entry vfile pos@(J.Position l c) = runMaybeT $ do
    ast <- liftMaybe entry.moduleAST
    let line = VFS.rangeLinesFromVfs vfile $ J.Range (J.Position l 0) (J.Position (l + 1) 0)
        c'   = snapToLastTokenEnd (T.unpack line) c
        pos' = J.Position l c'
    (sym, spi, args) <- liftMaybe
        $  findExpressionApplication store ast pos'
       <|> findTypeApplication       store ast pos'
    lift $ infoM $ "Found symbol " <> sym.qualIdent
    symEnd <- liftMaybe [end | J.Range _ end <- currySpanInfo2Range spi]
    let defaultParam | pos >= symEnd = fromIntegral $ length args
                     | otherwise     = 0
        activeParam = maybe defaultParam fst $ find (elementContains pos . snd) (zip [0..] args)
        activeSig = 0
        labelStart = sym.qualIdent <> " :: "
        paramSep = " -> "
        paramLabels = sym.printedArgumentTypes
        paramOffsets = reverse $ snd $ foldl (\(n, offs) lbl -> let n' = n + T.length lbl in (n' + T.length paramSep, (n, n') : offs)) (T.length labelStart, []) paramLabels
        params = flip J.ParameterInformation Nothing . J.InR . bimap fromIntegral fromIntegral <$> paramOffsets
        label = labelStart <> T.intercalate paramSep (paramLabels ++ maybeToList sym.printedResultType)
        sig = J.SignatureInformation label Nothing (Just params) (Just (J.InL activeParam))
        sigs = [sig]
    return $ J.SignatureHelp sigs (Just activeSig) (Just (J.InL activeParam))

findExpressionApplication :: I.IndexStore -> ModuleAST -> J.Position -> Maybe (I.Symbol, CSPI.SpanInfo, [CSPI.SpanInfo])
findExpressionApplication store ast pos = lastSafe $ do
    e <- elementsAt pos $ expressions ast
    let base N.:| args = appFull e
    sym <- maybeToList $ lookupBaseExpression store ast base
    return (sym, CSPI.getSpanInfo e, CSPI.getSpanInfo <$> args)

findTypeApplication :: I.IndexStore -> ModuleAST -> J.Position -> Maybe (I.Symbol, CSPI.SpanInfo, [CSPI.SpanInfo])
findTypeApplication store ast pos = lastSafe $ do
    e <- elementsAt pos $ typeExpressions ast
    let base N.:| args = typeAppFull e
    sym <- maybeToList $ lookupBaseTypeExpression store ast base
    return (sym, CSPI.getSpanInfo e, CSPI.getSpanInfo <$> args)

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
