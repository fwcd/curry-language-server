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
import qualified Curry.Syntax as CS
import qualified Base.CurryTypes as CCT
import qualified Base.Types as CT
import qualified Text.PrettyPrint as PP

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Uri (filePathToUri, uriToFilePath)
import Data.Maybe (fromMaybe, listToMaybe)
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
          -- TODO: It would be better to have the frontend expose this as a flag/tag instead.
          tags | "Unused" `T.isPrefixOf` text || "Unreferenced" `T.isPrefixOf` text = Just $ J.List [J.DtUnnecessary]
               | otherwise                                                          = Just $ J.List []
          related = Nothing

curryPos2Pos :: CP.Position -> Maybe J.Position
curryPos2Pos CP.NoPos = Nothing
curryPos2Pos CP.Position {..} = Just $ J.Position (fromIntegral line - 1) (fromIntegral column - 1)

curryPos2Uri :: CP.Position -> MaybeT IO J.Uri
curryPos2Uri CP.NoPos = liftMaybe Nothing
curryPos2Uri CP.Position {..} = liftIO $ filePathToUri file

curryPos2Location :: CP.Position -> MaybeT IO J.Location
curryPos2Location cp = do
    p <- liftMaybe $ curryPos2Pos cp
    uri <- curryPos2Uri cp
    return $ J.Location uri $ pointRange p

curryPos2LocationLink :: CP.Position -> MaybeT IO J.LocationLink
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

currySpan2Uri :: CSP.Span -> MaybeT IO J.Uri
currySpan2Uri CSP.NoSpan = liftMaybe Nothing
currySpan2Uri CSP.Span {..} = curryPos2Uri start

currySpan2Location :: CSP.Span -> MaybeT IO J.Location
currySpan2Location CSP.NoSpan = liftMaybe Nothing
currySpan2Location spn = do
    range <- liftMaybe $ currySpan2Range spn
    uri <- currySpan2Uri spn
    return $ J.Location uri range

currySpan2LocationLink :: CSP.Span -> MaybeT IO J.LocationLink
currySpan2LocationLink CSP.NoSpan = liftMaybe Nothing
currySpan2LocationLink spn = do
    range <- liftMaybe $ currySpan2Range spn
    uri <- currySpan2Uri spn
    return $ J.LocationLink Nothing uri range range

currySpans2LocationLink :: CSP.Span -> CSP.Span -> MaybeT IO J.LocationLink
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

currySpanInfo2Uri :: CSPI.HasSpanInfo a => a -> MaybeT IO J.Uri
currySpanInfo2Uri (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2Uri srcSpan
currySpanInfo2Uri _ = liftMaybe Nothing

currySpanInfo2Location :: CSPI.HasSpanInfo a => a -> MaybeT IO J.Location
currySpanInfo2Location (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2Location srcSpan
currySpanInfo2Location _ = liftMaybe Nothing

currySpanInfo2LocationLink :: CSPI.HasSpanInfo a => a -> MaybeT IO J.LocationLink
currySpanInfo2LocationLink (CSPI.getSpanInfo -> CSPI.SpanInfo {..}) = currySpan2LocationLink srcSpan
currySpanInfo2LocationLink _ = liftMaybe Nothing

currySpanInfos2LocationLink :: CSPI.HasSpanInfo a => a -> CSPI.SpanInfo -> MaybeT IO J.LocationLink
currySpanInfos2LocationLink (CSPI.getSpanInfo -> CSPI.NoSpanInfo) spi = currySpanInfo2LocationLink spi
currySpanInfos2LocationLink (CSPI.getSpanInfo -> CSPI.SpanInfo{srcSpan=srcSpan}) (CSPI.getSpanInfo -> CSPI.SpanInfo {srcSpan=destSpan}) = currySpans2LocationLink srcSpan destSpan
currySpanInfos2LocationLink _ _ = liftMaybe Nothing

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
ppTypeSchemeToText mid = T.pack . PP.render . CCT.ppTypeScheme mid

ppPredTypeToText :: CI.ModuleIdent -> CT.PredType -> T.Text
ppPredTypeToText mid = T.pack . PP.render . CCT.ppPredType mid

ppPatternToName :: CS.Pattern a -> T.Text
ppPatternToName pat = case pat of
    CS.VariablePattern _ _ ident      -> ppToText ident
    CS.InfixPattern _ _ _ ident _     -> ppToText ident
    CS.RecordPattern _ _ ident _      -> ppToText ident
    CS.TuplePattern _ ps              -> "(" <> (T.intercalate ", " $ ppPatternToName <$> ps) <> ")"
    CS.InfixFuncPattern _ _ _ ident _ -> ppToText ident
    _ -> "?"

makeDocumentSymbol :: T.Text -> J.SymbolKind -> Maybe J.Range -> Maybe [J.DocumentSymbol] -> J.DocumentSymbol
makeDocumentSymbol n k r cs = J.DocumentSymbol n Nothing k Nothing Nothing r' r' $ J.List <$> cs
    where r' = fromMaybe emptyRange r

class HasDocumentSymbols s where
    documentSymbols :: s -> [J.DocumentSymbol]

instance HasDocumentSymbols (CS.Module a) where
    documentSymbols (CS.Module spi _ _ ident _ _ decls) = [makeDocumentSymbol name symKind range $ Just childs]
        where name = ppToText ident
              symKind = J.SkModule
              range = currySpanInfo2Range spi
              childs = documentSymbols =<< decls

instance HasDocumentSymbols (CS.Decl a) where
    documentSymbols decl = case decl of
        CS.InfixDecl _ _ _ idents -> [makeDocumentSymbol name symKind range Nothing]
            where name = maybe "<infix operator>" ppToText $ listToMaybe idents
                  symKind = J.SkOperator
        CS.DataDecl _ ident _ cs _ -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if length cs > 1 then J.SkEnum
                                             else J.SkStruct
                  childs = documentSymbols =<< cs
        CS.NewtypeDecl _ ident _ c _ -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SkStruct
                  childs = documentSymbols c
        CS.ExternalDataDecl _ ident _ -> [makeDocumentSymbol name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkStruct
        CS.FunctionDecl _ _ ident eqs -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if eqsArity eqs > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = documentSymbols =<< eqs
        CS.TypeDecl _ ident _ _ -> [makeDocumentSymbol name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkInterface
        CS.ExternalDecl _ vars -> documentSymbols =<< vars
        CS.FreeDecl _ vars -> documentSymbols =<< vars
        CS.PatternDecl _ pat rhs -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppPatternToName pat
                  symKind = if patArity pat > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = documentSymbols rhs
        CS.ClassDecl _ _ _ ident _ decls -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = J.SkInterface
                  childs = documentSymbols =<< decls
        CS.InstanceDecl _ _ _ qident t decls -> [makeDocumentSymbol name symKind range $ Just childs]
            where name = ppToText qident <> " (" <> (T.pack $ PP.render $ CPP.pPrintPrec 2 t) <> ")"
                  symKind = J.SkNamespace
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
              eqsArity eqs = maybe 1 (\(CS.Equation _ lhs _) -> lhsArity lhs) $ listToMaybe eqs
              range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Var a) where
    documentSymbols (CS.Var _ ident) = [makeDocumentSymbol (ppToText ident) J.SkVariable range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo ident

instance HasDocumentSymbols CS.ConstrDecl where
    documentSymbols decl = case decl of
        CS.ConstrDecl _ ident _  -> [makeDocumentSymbol (ppToText ident) J.SkEnumMember range Nothing]
        CS.ConOpDecl _ _ ident _ -> [makeDocumentSymbol (ppToText ident) J.SkOperator range Nothing]
        CS.RecordDecl _ ident _  -> [makeDocumentSymbol (ppToText ident) J.SkEnumMember range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo decl

instance HasDocumentSymbols (CS.Equation a) where
    documentSymbols (CS.Equation _ _ rhs) = documentSymbols rhs

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
        where symKind = J.SkEnumMember

class HasWorkspaceSymbols s where
    workspaceSymbols :: s -> IO [J.SymbolInformation]

instance (HasDocumentSymbols s, CSPI.HasSpanInfo s) => HasWorkspaceSymbols s where
    workspaceSymbols s = do
        loc <- runMaybeT $ currySpanInfo2Location $ CSPI.getSpanInfo s
        let documentSymbolToInformations :: J.DocumentSymbol -> [J.SymbolInformation]
            documentSymbolToInformations (J.DocumentSymbol n _ k ts d _ _ cs) = ((\l -> J.SymbolInformation n k ts d l Nothing) <$> loc) `maybeCons` cis
                where cs' = maybe [] (\(J.List cs'') -> cs'') cs
                      cis = documentSymbolToInformations =<< cs'
        return $ documentSymbolToInformations =<< documentSymbols s

-- Language Server Protocol -> Curry Compiler

-- TODO
