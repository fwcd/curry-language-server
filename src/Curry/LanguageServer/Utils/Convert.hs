-- | Convert between Curry Compiler and language server structures
{-# LANGUAGE RecordWildCards, ViewPatterns, OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
module Curry.LanguageServer.Utils.Convert
    ( curryMsg2Diagnostic
    , curryPos2Pos
    , curryPos2Uri
    , curryPos2Location
    , curryPos2LocationLink
    , currySpan2Range
    , currySpan2Uri
    , currySpan2Location
    , currySpan2LocationLink
    , currySpans2LocationLink
    , currySpanInfo2Range
    , currySpanInfo2Uri
    , currySpanInfo2Location
    , currySpanInfo2LocationLink
    , currySpanInfos2LocationLink
    , curryTextEdit2TextEdit
    , curryTextEdit2TextDocumentEdit
    , curryTextEdit2WorkspaceEdit
    , curryQuickFix2CodeAction
    , setCurryPosUri
    , setCurrySpanUri
    , setCurrySpanInfoUri
    , ppToStringPrec
    , ppToTextPrec
    , ppToString
    , ppToText
    , ppTypeSchemeToText
    , ppPredTypeToText
    , HasDocumentSymbols (..)
    , HasWorkspaceSymbols (..)
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Base.Position as CP
import qualified Curry.Base.Pretty as CPP
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Base.TextEdit as CTE
import qualified Curry.Base.QuickFix as CQF
import qualified Curry.Syntax as CS
import qualified Curry.Frontend.Base.Types as CT
import qualified Text.PrettyPrint as PP

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Uri (filePathToUri, uriToFilePath)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J

-- Curry Compiler -> Language Server Protocol

curryMsg2Diagnostic :: J.DiagnosticSeverity -> CM.Message -> J.Diagnostic
curryMsg2Diagnostic s msg = J.Diagnostic range severity code codeDesc src text tags related dataValue
    where range = fromMaybe emptyRange $ currySpanInfo2Range $ CM.msgSpanInfo msg
          severity = Just s
          code = Nothing
          codeDesc = Nothing
          src = Nothing
          text = T.pack $ PP.render $ CM.msgTxt msg
          -- TODO: It would be better to have the frontend expose this as a flag/tag instead.
          tags | "Unused" `T.isPrefixOf` text || "Unreferenced" `T.isPrefixOf` text = Just [J.DiagnosticTag_Unnecessary]
               | otherwise                                                          = Just []
          related = Nothing
          dataValue = Nothing

curryPos2Pos :: CP.Position -> Maybe J.Position
curryPos2Pos CP.NoPos = Nothing
curryPos2Pos CP.Position {..} = Just $ J.Position (fromIntegral line - 1) (fromIntegral column - 1)

curryPos2Uri :: MonadIO m => CP.Position -> MaybeT m J.Uri
curryPos2Uri CP.NoPos = liftMaybe Nothing
curryPos2Uri CP.Position {..} = filePathToUri file

curryPos2Location :: MonadIO m => CP.Position -> MaybeT m J.Location
curryPos2Location cp = do
    p <- liftMaybe $ curryPos2Pos cp
    uri <- curryPos2Uri cp
    return $ J.Location uri $ pointRange p

curryPos2LocationLink :: MonadIO m => CP.Position -> MaybeT m J.LocationLink
curryPos2LocationLink cp = do
    p <- liftMaybe $ curryPos2Pos cp
    uri <- curryPos2Uri cp
    let range = pointRange p
    return $ J.LocationLink Nothing uri range range

currySpan2Range :: CSP.Span -> Maybe J.Range
currySpan2Range CSP.NoSpan = Nothing
currySpan2Range CSP.Span {..} = do
    s <- curryPos2Pos start
    J.Position el ec <- curryPos2Pos end
    return $ J.Range s $ J.Position el (ec + 1)

currySpan2Uri :: MonadIO m => CSP.Span -> MaybeT m J.Uri
currySpan2Uri CSP.NoSpan = liftMaybe Nothing
currySpan2Uri CSP.Span {..} = curryPos2Uri start

currySpan2Location :: MonadIO m => CSP.Span -> MaybeT m J.Location
currySpan2Location CSP.NoSpan = liftMaybe Nothing
currySpan2Location spn = do
    range <- liftMaybe $ currySpan2Range spn
    uri <- currySpan2Uri spn
    return $ J.Location uri range

currySpan2LocationLink :: MonadIO m => CSP.Span -> MaybeT m J.LocationLink
currySpan2LocationLink CSP.NoSpan = liftMaybe Nothing
currySpan2LocationLink spn = do
    range <- liftMaybe $ currySpan2Range spn
    uri <- currySpan2Uri spn
    return $ J.LocationLink Nothing uri range range

currySpans2LocationLink :: MonadIO m => CSP.Span -> CSP.Span -> MaybeT m J.LocationLink
currySpans2LocationLink CSP.NoSpan destSpan = currySpan2LocationLink destSpan
currySpans2LocationLink _ CSP.NoSpan = liftMaybe Nothing
currySpans2LocationLink srcSpan destSpan = do
    srcRange <- liftMaybe $ currySpan2Range srcSpan
    destRange <- liftMaybe $ currySpan2Range destSpan
    uri <- currySpan2Uri destSpan
    return $ J.LocationLink (Just srcRange) uri destRange destRange

currySpanInfo2Range :: CSPI.HasSpanInfo a => a -> Maybe J.Range
currySpanInfo2Range (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2Range srcSpan
currySpanInfo2Range _ = Nothing

currySpanInfo2Uri :: MonadIO m => CSPI.HasSpanInfo a => a -> MaybeT m J.Uri
currySpanInfo2Uri (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2Uri srcSpan
currySpanInfo2Uri _ = liftMaybe Nothing

currySpanInfo2Location :: MonadIO m => CSPI.HasSpanInfo a => a -> MaybeT m J.Location
currySpanInfo2Location (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2Location srcSpan
currySpanInfo2Location _ = liftMaybe Nothing

currySpanInfo2LocationLink :: MonadIO m => CSPI.HasSpanInfo a => a -> MaybeT m J.LocationLink
currySpanInfo2LocationLink (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2LocationLink srcSpan
currySpanInfo2LocationLink _ = liftMaybe Nothing

currySpanInfos2LocationLink :: MonadIO m => CSPI.HasSpanInfo a => a -> CSPI.SpanInfo -> MaybeT m J.LocationLink
currySpanInfos2LocationLink (CSPI.getSpanInfo -> CSPI.NoSpanInfo) spi = currySpanInfo2LocationLink spi
currySpanInfos2LocationLink (CSPI.getSpanInfo -> CSPI.SpanInfo{srcSpan=srcSpan}) (CSPI.getSpanInfo -> CSPI.SpanInfo {srcSpan=destSpan}) = currySpans2LocationLink srcSpan destSpan
currySpanInfos2LocationLink _ _ = liftMaybe Nothing

curryTextEdit2TextEdit :: MonadIO m => CTE.TextEdit -> MaybeT m J.TextEdit
curryTextEdit2TextEdit (CTE.TextEdit s e t) = do
    s' <- liftMaybe $ curryPos2Pos s
    e' <- liftMaybe $ curryPos2Pos e
    let range = J.Range s' e'
    return $ J.TextEdit range (T.pack t)

curryTextEdit2TextDocumentEdit :: MonadIO m => CTE.TextEdit -> MaybeT m J.TextDocumentEdit
curryTextEdit2TextDocumentEdit e = do
    uri <- curryPos2Uri $ CTE.editStart e
    let doc = J.OptionalVersionedTextDocumentIdentifier uri $ J.InL 0
    tedit <- curryTextEdit2TextEdit e
    return $ J.TextDocumentEdit doc [J.InL tedit]

curryTextEdit2WorkspaceEdit :: MonadIO m => CTE.TextEdit -> MaybeT m J.WorkspaceEdit
curryTextEdit2WorkspaceEdit e = do
    docEdit <- curryTextEdit2TextDocumentEdit e
    return $ J.WorkspaceEdit Nothing (Just [J.InL docEdit]) Nothing

curryQuickFix2CodeAction :: MonadIO m => CQF.QuickFix -> [J.Diagnostic] -> MaybeT m J.CodeAction
curryQuickFix2CodeAction (CQF.QuickFix e desc) diags = do
    wedit <- curryTextEdit2WorkspaceEdit e
    return $ J.CodeAction (T.pack desc) (Just kind) (Just diags) Nothing Nothing (Just wedit) Nothing Nothing
    where kind = J.CodeActionKind_QuickFix

setCurryPosUri :: CP.HasPosition a => J.Uri -> a -> Maybe a
setCurryPosUri uri x@(CP.getPosition -> p@(CP.Position {})) = do
    fp <- uriToFilePath uri
    return $ CP.setPosition p { CP.file = fp } x
setCurryPosUri _ x = Just x

setCurrySpanUri :: J.Uri -> CSP.Span -> Maybe CSP.Span
setCurrySpanUri uri CSP.Span {..} = do
    fp <- uriToFilePath uri
    p1 <- setCurryPosUri uri start
    p2 <- setCurryPosUri uri end
    return $ CSP.Span fp p1 p2
setCurrySpanUri _ CSP.NoSpan = Just CSP.NoSpan

setCurrySpanInfoUri :: CSPI.HasSpanInfo a => J.Uri -> a -> Maybe a
setCurrySpanInfoUri uri x@(CSPI.getSpanInfo -> spi@CSPI.SpanInfo {..}) = do
    spn <- setCurrySpanUri uri srcSpan
    return $ CSPI.setSpanInfo spi { CSPI.srcSpan = spn } x
setCurrySpanInfoUri _ x = Just x

ppToStringPrec :: CPP.Pretty p => Int -> p -> String
ppToStringPrec p = PP.render . CPP.pPrintPrec p

ppToTextPrec :: CPP.Pretty p => Int -> p -> T.Text
ppToTextPrec p = T.pack . ppToStringPrec p

ppToString :: CPP.Pretty p => p -> String
ppToString = PP.render . CPP.pPrint

ppToText :: CPP.Pretty p => p -> T.Text
ppToText = T.pack . ppToString

ppTypeSchemeToText :: CI.ModuleIdent -> CT.TypeScheme -> T.Text
ppTypeSchemeToText mid = T.pack . PP.render . CT.ppTypeScheme mid

ppPredTypeToText :: CI.ModuleIdent -> CT.PredType -> T.Text
ppPredTypeToText mid = T.pack . PP.render . CT.ppPredType mid

ppPatternToName :: CS.Pattern a -> T.Text
ppPatternToName pat = case pat of
    CS.VariablePattern _ _ ident      -> ppToText ident
    CS.InfixPattern _ _ _ ident _     -> ppToText ident
    CS.RecordPattern _ _ ident _      -> ppToText ident
    CS.TuplePattern _ ps              -> "(" <> (T.intercalate ", " $ ppPatternToName <$> ps) <> ")"
    CS.InfixFuncPattern _ _ _ ident _ -> ppToText ident
    _ -> "?"

makeDocumentSymbol :: T.Text -> J.SymbolKind -> Maybe J.Range -> Maybe [J.DocumentSymbol] -> J.DocumentSymbol
makeDocumentSymbol n k r cs = J.DocumentSymbol n Nothing k Nothing Nothing r' r' cs
    where r' = fromMaybe emptyRange r

class HasDocumentSymbols s where
    documentSymbols :: s -> [J.DocumentSymbol]

instance HasDocumentSymbols (CS.Module a) where
    documentSymbols (CS.Module spi _ _ ident _ _ decls) = [makeDocumentSymbol name symKind range $ Just childs]
        where name = ppToText ident
              symKind = J.SymbolKind_Module
              range = currySpanInfo2Range spi
              childs = documentSymbols =<< decls

instance HasDocumentSymbols (CS.Decl a) where
    documentSymbols decl = case decl of
        CS.InfixDecl _ _ _ idents -> [makeDocumentSymbol name symKind range Nothing]
            where name = maybe "<infix operator>" ppToText $ listToMaybe idents
                  symKind = J.SymbolKind_Operator
        CS.DataDecl _ ident _ cs _ -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if length cs > 1 then J.SymbolKind_Enum
                                             else J.SymbolKind_Struct
                  childs = documentSymbols =<< cs
        CS.NewtypeDecl _ ident _ c _ -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SymbolKind_Struct
                  childs = documentSymbols c
        CS.ExternalDataDecl _ ident _ -> [makeDocumentSymbol name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SymbolKind_Struct
        CS.FunctionDecl _ _ ident eqs -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if eqsArity eqs > 0 then J.SymbolKind_Function
                                                else J.SymbolKind_Constant
                  childs = documentSymbols =<< eqs
        CS.TypeDecl _ ident _ _ -> [makeDocumentSymbol name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SymbolKind_Interface
        CS.ExternalDecl _ vars -> documentSymbols =<< vars
        CS.FreeDecl _ vars -> documentSymbols =<< vars
        CS.PatternDecl _ pat rhs -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppPatternToName pat
                  symKind = if patArity pat > 0 then J.SymbolKind_Function
                                                else J.SymbolKind_Constant
                  childs = documentSymbols rhs
        CS.ClassDecl _ _ _ ident _ _ decls -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SymbolKind_Interface
                  childs = documentSymbols =<< decls
        CS.InstanceDecl _ _ _ qident t decls -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText qident <> " (" <> (T.pack $ PP.render $ CPP.pPrintPrec 2 t) <> ")"
                  symKind = J.SymbolKind_Namespace
                  childs = documentSymbols =<< decls
        _ -> []
        where lhsArity :: CS.Lhs a -> Int
              lhsArity lhs = case lhs of
                  CS.FunLhs _ _ pats -> length pats
                  CS.OpLhs _ _ _ _   -> 2
                  CS.ApLhs _ _ pats  -> length pats
              patArity :: CS.Pattern a -> Int
              patArity pat = case pat of
                  CS.FunctionPattern _ _ _ ps -> length ps
                  _                           -> 0
              eqsArity :: [CS.Equation a] -> Int
              eqsArity eqs = maybe 1 (\(CS.Equation _ _ lhs _) -> lhsArity lhs) $ listToMaybe eqs
              range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Var a) where
    documentSymbols (CS.Var _ ident) = [makeDocumentSymbol (ppToText ident) J.SymbolKind_Variable range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo ident

instance HasDocumentSymbols CS.ConstrDecl where
    documentSymbols decl = case decl of
        CS.ConstrDecl _ ident _  -> [makeDocumentSymbol (ppToText ident) J.SymbolKind_EnumMember range Nothing]
        CS.ConOpDecl _ _ ident _ -> [makeDocumentSymbol (ppToText ident) J.SymbolKind_Operator range Nothing]
        CS.RecordDecl _ ident _  -> [makeDocumentSymbol (ppToText ident) J.SymbolKind_EnumMember range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Equation a) where
    documentSymbols (CS.Equation _ _ _ rhs) = documentSymbols rhs

instance HasDocumentSymbols (CS.Rhs a) where
    documentSymbols rhs = case rhs of
        CS.SimpleRhs _ _ e decls      -> documentSymbols e ++ (documentSymbols =<< decls)
        CS.GuardedRhs _ _ conds decls -> (documentSymbols =<< conds) ++ (documentSymbols =<< decls)

instance HasDocumentSymbols (CS.CondExpr a) where
    documentSymbols (CS.CondExpr _ e1 e2) = documentSymbols e1 ++ documentSymbols e2

instance HasDocumentSymbols (CS.Expression a) where
    documentSymbols e = case e of
        CS.Paren _ e'                -> documentSymbols e'
        CS.Typed _ e' _              -> documentSymbols e'
        CS.Record _ _ _ fields       -> fieldSymbols =<< fields
        CS.RecordUpdate _ e' fields  -> documentSymbols e' ++ (fieldSymbols =<< fields)
        CS.Tuple _ entries           -> documentSymbols =<< entries
        CS.List _ _ entries          -> documentSymbols =<< entries
        CS.ListCompr _ e' stmts      -> documentSymbols e' ++ (documentSymbols =<< stmts)
        CS.EnumFrom _ e'             -> documentSymbols e'
        CS.EnumFromThen _ e1 e2      -> documentSymbols e1 ++ documentSymbols e2
        CS.EnumFromThenTo _ e1 e2 e3 -> documentSymbols e1 ++ documentSymbols e2 ++ documentSymbols e3
        CS.UnaryMinus _ e'           -> documentSymbols e'
        CS.Apply _ e1 e2             -> documentSymbols e1 ++ documentSymbols e2
        CS.InfixApply _ e1 _ e2      -> documentSymbols e1 ++ documentSymbols e2
        CS.LeftSection _ e' _        -> documentSymbols e'
        CS.RightSection _ _ e'       -> documentSymbols e'
        CS.Lambda _ _ e'             -> documentSymbols e'
        CS.Let _ _ decls e'          -> (documentSymbols =<< decls) ++ documentSymbols e'
        CS.Do _ _ stmts e'           -> (documentSymbols =<< stmts) ++ documentSymbols e'
        CS.IfThenElse _ e1 e2 e3     -> documentSymbols e1 ++ documentSymbols e2 ++ documentSymbols e3
        CS.Case _ _ _ e' alts        -> documentSymbols e' ++ (documentSymbols =<< alts)
        _                            -> []
        where fieldSymbols (CS.Field _ _ e') = documentSymbols e'

instance HasDocumentSymbols (CS.Statement a) where
    documentSymbols stmt = case stmt of
        CS.StmtExpr _ e       -> documentSymbols e
        CS.StmtDecl _ _ decls -> documentSymbols =<< decls
        CS.StmtBind _ _ e     -> documentSymbols e

instance HasDocumentSymbols (CS.Alt a) where
    documentSymbols (CS.Alt _ _ rhs) = documentSymbols rhs

instance HasDocumentSymbols CS.NewConstrDecl where
    documentSymbols decl = case decl of
        CS.NewConstrDecl spi ident _ -> [makeDocumentSymbol (ppToText ident) symKind (currySpanInfo2Range spi) Nothing]
        CS.NewRecordDecl spi ident _ -> [makeDocumentSymbol (ppToText ident) symKind (currySpanInfo2Range spi) Nothing]
        where symKind = J.SymbolKind_EnumMember

class HasWorkspaceSymbols s where
    workspaceSymbols :: s -> IO [J.SymbolInformation]

instance (HasDocumentSymbols s, CSPI.HasSpanInfo s) => HasWorkspaceSymbols s where
    workspaceSymbols s = do
        loc <- runMaybeT $ currySpanInfo2Location $ CSPI.getSpanInfo s
        let documentSymbolToInformations :: J.DocumentSymbol -> [J.SymbolInformation]
            documentSymbolToInformations (J.DocumentSymbol n _ k ts d _ _ cs) = ((\l -> J.SymbolInformation n k ts Nothing d l) <$> loc) `maybeCons` cis
                where cs' = maybe [] id cs
                      cis = documentSymbolToInformations =<< cs'
        return $ documentSymbolToInformations =<< documentSymbols s

-- Language Server Protocol -> Curry Compiler

-- TODO
