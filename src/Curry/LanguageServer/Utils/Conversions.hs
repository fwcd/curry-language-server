-- | Conversions between Curry Compiler and language server structures
{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
module Curry.LanguageServer.Utils.Conversions (
    curryMsg2Diagnostic,
    curryPos2Pos,
    curryPos2Uri,
    curryPos2Location,
    currySpan2Range,
    currySpan2Location,
    currySpan2Uri,
    currySpanInfo2Range,
    currySpanInfo2Location,
    currySpanInfo2Uri,
    ppToText,
    HasDocumentSymbols (..),
    HasSymbolKind (..),
    HasCodeLenses (..),
    HasWorkspaceSymbols (..),
    bindingToQualSymbols
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Base.Position as CP
import qualified Curry.Base.Pretty as CPP
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV
import qualified Text.PrettyPrint as PP

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Uri
import Data.Maybe
import qualified Data.Text as T
import qualified Language.LSP.Types as J

-- Curry Compiler -> Language Server Protocol

curryMsg2Diagnostic :: J.DiagnosticSeverity -> CM.Message -> J.Diagnostic
curryMsg2Diagnostic s msg = J.Diagnostic range severity code src text tags related
    where range = fromMaybe emptyRange $ currySpanInfo2Range $ CM.msgSpanInfo msg
          severity = Just s
          code = Nothing
          src = Nothing
          text = T.pack $ PP.render $ CM.msgTxt msg
          tags = Nothing
          related = Nothing

curryPos2Pos :: CP.Position -> Maybe J.Position
curryPos2Pos CP.NoPos = Nothing
curryPos2Pos CP.Position {..} = Just $ J.Position (line - 1) (column - 1)

curryPos2Uri :: CP.Position -> MaybeT IO J.Uri
curryPos2Uri CP.NoPos = liftMaybe Nothing
curryPos2Uri CP.Position {..} = liftIO $ filePathToUri file

curryPos2Location :: CP.Position -> MaybeT IO J.Location
curryPos2Location cp = do
    p <- liftMaybe $ curryPos2Pos cp
    uri <- curryPos2Uri cp
    return $ J.Location uri $ pointRange p

currySpan2Range :: CSP.Span -> Maybe J.Range
currySpan2Range CSP.NoSpan = Nothing
currySpan2Range CSP.Span {..} = do
    s <- curryPos2Pos start
    J.Position el ec <- curryPos2Pos end
    return $ J.Range s $ J.Position el (ec + 1)

currySpan2Location :: CSP.Span -> MaybeT IO J.Location
currySpan2Location CSP.NoSpan = liftMaybe Nothing
currySpan2Location spn = do
    range <- liftMaybe $ currySpan2Range spn
    uri <- curryPos2Uri $ CSP.start spn
    return $ J.Location uri range

currySpan2Uri :: CSP.Span -> MaybeT IO J.Uri
currySpan2Uri CSP.NoSpan = liftMaybe Nothing
currySpan2Uri CSP.Span {..} = curryPos2Uri start

currySpanInfo2Range :: CSPI.SpanInfo -> Maybe J.Range
currySpanInfo2Range CSPI.NoSpanInfo = Nothing
currySpanInfo2Range CSPI.SpanInfo {..} = currySpan2Range srcSpan

currySpanInfo2Location :: CSPI.SpanInfo -> MaybeT IO J.Location
currySpanInfo2Location CSPI.NoSpanInfo = liftMaybe Nothing
currySpanInfo2Location CSPI.SpanInfo {..} = currySpan2Location srcSpan

currySpanInfo2Uri :: CSPI.SpanInfo -> MaybeT IO J.Uri
currySpanInfo2Uri CSPI.NoSpanInfo = liftMaybe Nothing
currySpanInfo2Uri CSPI.SpanInfo {..} = currySpan2Uri srcSpan

ppToText :: CPP.Pretty p => p -> T.Text
ppToText = T.pack . PP.render . CPP.pPrint

ppPatternToName :: CS.Pattern a -> T.Text
ppPatternToName pat = case pat of
    CS.VariablePattern _ _ ident      -> ppToText ident
    CS.InfixPattern _ _ _ ident _     -> ppToText ident
    CS.RecordPattern _ _ ident _      -> ppToText ident
    CS.TuplePattern _ ps              -> "(" <> (T.intercalate ", " $ ppPatternToName <$> ps) <> ")"
    CS.InfixFuncPattern _ _ _ ident _ -> ppToText ident
    _ -> "?"

documentSymbolFrom :: T.Text -> J.SymbolKind -> Maybe J.Range -> Maybe [J.DocumentSymbol] -> J.DocumentSymbol
documentSymbolFrom n k r cs = J.DocumentSymbol n Nothing k Nothing r' r' $ J.List <$> cs
    where r' = fromMaybe emptyRange r

symbolInformationFrom :: T.Text -> J.SymbolKind -> Maybe J.Location -> Maybe J.SymbolInformation
symbolInformationFrom n k = ((\l -> J.SymbolInformation n k Nothing l Nothing) <$>)

class HasDocumentSymbols s where
    documentSymbols :: s -> [J.DocumentSymbol]

instance HasDocumentSymbols (CS.Module a) where
    documentSymbols (CS.Module spi _ _ ident _ _ decls) = [documentSymbolFrom name symKind range $ Just childs]
        where name = ppToText ident
              symKind = J.SkModule
              range = currySpanInfo2Range spi
              childs = decls >>= documentSymbols

instance HasDocumentSymbols (CS.Decl a) where
    documentSymbols decl = case decl of
        CS.InfixDecl _ _ _ idents -> [documentSymbolFrom name symKind range Nothing]
            where name = maybe "<infix operator>" ppToText $ listToMaybe idents
                  symKind = J.SkOperator
        CS.DataDecl _ ident _ cs _ -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if length cs > 1 then J.SkEnum
                                             else J.SkStruct
                  childs = cs >>= documentSymbols
        CS.NewtypeDecl _ ident _ c _ -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SkStruct
                  childs = documentSymbols c
        CS.ExternalDataDecl _ ident _ -> [documentSymbolFrom name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkStruct
        CS.FunctionDecl _ _ ident eqs -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if eqsArity eqs > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = eqs >>= documentSymbols
        CS.TypeDecl _ ident _ _ -> [documentSymbolFrom name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkInterface
        CS.ExternalDecl _ vars -> vars >>= documentSymbols
        CS.FreeDecl _ vars -> vars >>= documentSymbols
        CS.PatternDecl _ pat rhs -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppPatternToName pat
                  symKind = if patArity pat > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = documentSymbols rhs
        CS.ClassDecl _ _ _ ident _ decls -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SkInterface
                  childs = decls >>= documentSymbols
        CS.InstanceDecl _ _ _ qident t decls -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText qident <> " (" <> (T.pack $ PP.render $ CPP.pPrintPrec 2 t) <> ")"
                  symKind = J.SkNamespace
                  childs = decls >>= documentSymbols
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
              eqsArity eqs = maybe 1 (\(CS.Equation _ lhs _) -> lhsArity lhs) $ listToMaybe eqs
              range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Var a) where
    documentSymbols (CS.Var _ ident) = [documentSymbolFrom (ppToText ident) J.SkVariable range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo ident

instance HasDocumentSymbols CS.ConstrDecl where
    documentSymbols decl = case decl of
        CS.ConstrDecl _ ident _  -> [documentSymbolFrom (ppToText ident) J.SkEnumMember range Nothing]
        CS.ConOpDecl _ _ ident _ -> [documentSymbolFrom (ppToText ident) J.SkOperator range Nothing]
        CS.RecordDecl _ ident _  -> [documentSymbolFrom (ppToText ident) J.SkEnumMember range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Equation a) where
    documentSymbols (CS.Equation _ _ rhs) = documentSymbols rhs

instance HasDocumentSymbols (CS.Rhs a) where
    documentSymbols rhs = case rhs of
        CS.SimpleRhs _ _ e decls      -> documentSymbols e ++ (decls >>= documentSymbols)
        CS.GuardedRhs _ _ conds decls -> (conds >>= documentSymbols) ++ (decls >>= documentSymbols)

instance HasDocumentSymbols (CS.CondExpr a) where
    documentSymbols (CS.CondExpr _ e1 e2) = documentSymbols e1 ++ documentSymbols e2

instance HasDocumentSymbols (CS.Expression a) where
    documentSymbols e = case e of
        CS.Paren _ e'                -> documentSymbols e'
        CS.Typed _ e' _              -> documentSymbols e'
        CS.Record _ _ _ fields       -> fields >>= fieldSymbols
        CS.RecordUpdate _ e' fields  -> documentSymbols e' ++ (fields >>= fieldSymbols)
        CS.Tuple _ entries           -> entries >>= documentSymbols
        CS.List _ _ entries          -> entries >>= documentSymbols
        CS.ListCompr _ e' stmts      -> documentSymbols e' ++ (stmts >>= documentSymbols)
        CS.EnumFrom _ e'             -> documentSymbols e'
        CS.EnumFromThen _ e1 e2      -> documentSymbols e1 ++ documentSymbols e2
        CS.EnumFromThenTo _ e1 e2 e3 -> documentSymbols e1 ++ documentSymbols e2 ++ documentSymbols e3
        CS.UnaryMinus _ e'           -> documentSymbols e'
        CS.Apply _ e1 e2             -> documentSymbols e1 ++ documentSymbols e2
        CS.InfixApply _ e1 _ e2      -> documentSymbols e1 ++ documentSymbols e2
        CS.LeftSection _ e' _        -> documentSymbols e'
        CS.RightSection _ _ e'       -> documentSymbols e'
        CS.Lambda _ _ e'             -> documentSymbols e'
        CS.Let _ _ decls e'          -> (decls >>= documentSymbols) ++ documentSymbols e'
        CS.Do _ _ stmts e'           -> (stmts >>= documentSymbols) ++ documentSymbols e'
        CS.IfThenElse _ e1 e2 e3     -> documentSymbols e1 ++ documentSymbols e2 ++ documentSymbols e3
        CS.Case _ _ _ e' alts        -> documentSymbols e' ++ (alts >>= documentSymbols)
        _                            -> []
        where fieldSymbols (CS.Field _ _ e') = documentSymbols e'

instance HasDocumentSymbols (CS.Statement a) where
    documentSymbols stmt = case stmt of
        CS.StmtExpr _ e       -> documentSymbols e
        CS.StmtDecl _ _ decls -> decls >>= documentSymbols
        CS.StmtBind _ _ e     -> documentSymbols e

instance HasDocumentSymbols (CS.Alt a) where
    documentSymbols (CS.Alt _ _ rhs) = documentSymbols rhs

instance HasDocumentSymbols CS.NewConstrDecl where
    documentSymbols decl = case decl of
        CS.NewConstrDecl spi ident _ -> [documentSymbolFrom (ppToText ident) symKind (currySpanInfo2Range spi) Nothing]
        CS.NewRecordDecl spi ident _ -> [documentSymbolFrom (ppToText ident) symKind (currySpanInfo2Range spi) Nothing]
        where symKind = J.SkEnumMember

class HasSymbolKind s where
    symbolKind :: s -> J.SymbolKind
    
instance HasSymbolKind CEV.ValueInfo where
    symbolKind vinfo = case vinfo of
        CEV.DataConstructor _ _ _ _  -> J.SkEnumMember
        CEV.NewtypeConstructor _ _ _ -> J.SkEnumMember
        CEV.Value _ _ arity _        -> if arity > 0 then J.SkFunction
                                                     else J.SkConstant
        CEV.Label _ _ _              -> J.SkFunction -- Arity is always 1 for record labels

instance HasSymbolKind CETC.TypeInfo where
    symbolKind tinfo = case tinfo of
        CETC.DataType _ _ _     -> J.SkStruct
        CETC.RenamingType _ _ _ -> J.SkInterface
        CETC.AliasType _ _ _ _  -> J.SkInterface
        CETC.TypeClass _ _ _    -> J.SkInterface
        CETC.TypeVar _          -> J.SkTypeParameter

class HasCodeLenses s where
    codeLenses :: s -> [J.CodeLens]

instance HasCodeLenses (CS.Module a) where
    codeLenses = const [] -- TODO

class HasWorkspaceSymbols s where
    workspaceSymbols :: s -> IO [J.SymbolInformation]

instance (HasDocumentSymbols s, CSPI.HasSpanInfo s) => HasWorkspaceSymbols s where
    workspaceSymbols s = do
        loc <- runMaybeT $ currySpanInfo2Location $ CSPI.getSpanInfo s
        let documentSymbolToInformations :: J.DocumentSymbol -> [J.SymbolInformation]
            documentSymbolToInformations (J.DocumentSymbol n _ k d _ _ cs) = ((\l -> J.SymbolInformation n k d l Nothing) <$> loc) `maybeCons` cis
                where cs' = maybe [] (\(J.List cs'') -> cs'') cs
                      cis = documentSymbolToInformations =<< cs'
        return $ documentSymbolToInformations =<< documentSymbols s

bindingToQualSymbols :: HasSymbolKind k => (CI.QualIdent, k) -> IO [(CI.QualIdent, J.SymbolInformation)]
bindingToQualSymbols (qident, v) = do
    loc <- runMaybeT $ currySpanInfo2Location $ CI.qidSpanInfo qident
    let name = T.pack $ CI.idName $ CI.qidIdent qident
        symKind = symbolKind v
    return $ maybeToList $ pair qident <$> symbolInformationFrom name symKind loc

-- Language Server Protocol -> Curry Compiler

-- TODO
