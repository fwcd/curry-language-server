{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Utils.Conversions (
    curryMsg2Diagnostic,
    curryPos2Pos,
    currySpanInfo2Range,
    currySpan2Range,
    HasDocumentSymbols (..)
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Base.Position as CP
import qualified Curry.Base.Pretty as CPP
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS
import qualified Curry.Syntax.Pretty as CPP
import qualified Text.PrettyPrint as PP

import Data.Maybe
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import System.FilePath

-- Curry Compiler -> Language Server Protocol

curryMsg2Diagnostic :: J.DiagnosticSeverity -> CM.Message -> J.Diagnostic
curryMsg2Diagnostic s msg = J.Diagnostic range severity code src text related
    where pos@(J.Position ln col) = maybe (J.Position 0 0) id $ curryPos2Pos =<< CM.msgPos msg
          -- TODO: Fetch a span from Curry compiler instead of just a position
          range = J.Range pos $ J.Position ln (col + 200)
          severity = Just s
          code = Nothing
          src = Nothing
          text = T.pack $ PP.render $ CM.msgTxt msg
          related = Nothing

-- TODO: Use (file :: FilePath) from Curry Position to accurately
--       map diagnostics to their respective files.
curryPos2Pos :: CP.Position -> Maybe J.Position
curryPos2Pos CP.NoPos = Nothing
curryPos2Pos CP.Position {..} = Just $ J.Position (line - 1) (column - 1)

currySpanInfo2Range :: CSPI.SpanInfo -> Maybe J.Range
currySpanInfo2Range CSPI.NoSpanInfo = Nothing
currySpanInfo2Range CSPI.SpanInfo {..} = currySpan2Range srcSpan

currySpan2Range :: CSP.Span -> Maybe J.Range
currySpan2Range CSP.NoSpan = Nothing
currySpan2Range CSP.Span {..} = do
    s <- curryPos2Pos start
    e <- curryPos2Pos end
    return $ J.Range s e

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

emptyRange :: J.Range
emptyRange = J.Range (J.Position 0 0) (J.Position 0 0)

documentSymbolFrom :: T.Text -> J.SymbolKind -> Maybe J.Range -> Maybe [J.DocumentSymbol] -> J.DocumentSymbol
documentSymbolFrom n k r cs = J.DocumentSymbol n Nothing k Nothing r' r' $ J.List <$> cs
    where r' = maybe emptyRange id r

class HasDocumentSymbols s where
    documentSymbols :: s -> [J.DocumentSymbol]

instance HasDocumentSymbols (CS.Module a) where
    documentSymbols (CS.Module _ _ _ _ _ decls) = decls >>= documentSymbols -- TODO: Has seven arguments in later curry-base versions

instance HasDocumentSymbols (CS.Decl a) where
    documentSymbols decl = case decl of
        CS.InfixDecl _ _ _ idents -> [documentSymbolFrom name symKind range Nothing]
            where name = maybe "<infix operator>" ppToText $ listToMaybe idents
                  symKind = J.SkOperator
        CS.DataDecl _ ident _ cs _ -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if length cs > 1 then J.SkEnum
                                             else J.SkClass
                  childs = cs >>= documentSymbols
        CS.ExternalDataDecl _ ident _ -> [documentSymbolFrom name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkClass
        CS.FunctionDecl _ _ ident eqs -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppToText ident
                  symKind = if eqsArity eqs > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = eqs >>= documentSymbols
        CS.TypeDecl _ ident _ _ -> [documentSymbolFrom name symKind range Nothing]
            where name = ppToText ident
                  symKind = J.SkInterface
        -- TODO: 'Var' is currently not exported by Curry.Syntax.Type
        -- CS.ExternalDecl _ vars -> map varSymbol vars
        CS.PatternDecl _ pat rhs -> [documentSymbolFrom name symKind range $ Just childs]
            where name = ppPatternToName pat
                  symKind = if patArity pat > 0 then J.SkFunction
                                                else J.SkConstant
                  childs = documentSymbols rhs
        -- TODO: 'Var' is currently not exported by Curry.Syntax.Type
        -- CS.FreeDecl _ vars -> map varSymbol vars
        CS.ClassDecl _ _ _ ident decls -> [documentSymbolFrom name symKind range $ Just childs] -- TODO: Has another argument in later curry-base
            where name = ppToText ident
                  symKind = J.SkInterface
                  childs = decls >>= documentSymbols
        _ -> [] -- TODO
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
              -- TODO: 'Var' is currently not exported by Curry.Syntax.Type
              -- varSymbol (J.Var _ ident) = documentSymbolFrom (ppToText ident) J.SkVariable range Nothing

instance HasDocumentSymbols CS.ConstrDecl where
    documentSymbols decl = case decl of
        CS.ConstrDecl _ ident _  -> [documentSymbolFrom (ppToText ident) J.SkEnumMember range Nothing]
        CS.ConOpDecl _ _ ident _ -> [documentSymbolFrom (ppToText ident) J.SkOperator range Nothing]
        CS.RecordDecl _ ident _  -> [documentSymbolFrom (ppToText ident) J.SkEnumMember range Nothing]
        where range = currySpanInfo2Range $ CSPI.getSpanInfo decl
              rangeOrDefault = maybe emptyRange id range

instance HasDocumentSymbols (CS.Equation a) where
    documentSymbols (CS.Equation _ _ rhs) = documentSymbols rhs

instance HasDocumentSymbols (CS.Rhs a) where
    documentSymbols rhs = case rhs of
        CS.SimpleRhs _ e decls      -> (documentSymbols e) ++ (decls >>= documentSymbols)
        CS.GuardedRhs _ conds decls -> (conds >>= documentSymbols) ++ (decls >>= documentSymbols)

instance HasDocumentSymbols (CS.CondExpr a) where
    documentSymbols (CS.CondExpr _ e1 e2) = (documentSymbols e1) ++ (documentSymbols e2)

instance HasDocumentSymbols (CS.Expression a) where
    documentSymbols e = case e of
        CS.Paren _ e'                -> documentSymbols e'
        CS.Typed _ e' _              -> documentSymbols e'
        CS.Record _ _ _ fields       -> fields >>= fieldSymbols
        CS.RecordUpdate _ e' fields  -> (documentSymbols e') ++ (fields >>= fieldSymbols)
        CS.Tuple _ entries           -> entries >>= documentSymbols
        CS.List _ _ entries          -> entries >>= documentSymbols
        CS.ListCompr _ e' stmts      -> (documentSymbols e') ++ (stmts >>= documentSymbols)
        CS.EnumFrom _ e'             -> documentSymbols e'
        CS.EnumFromThen _ e1 e2      -> (documentSymbols e1) ++ (documentSymbols e2)
        CS.EnumFromThenTo _ e1 e2 e3 -> (documentSymbols e1) ++ (documentSymbols e2) ++ (documentSymbols e3)
        CS.UnaryMinus _ e'           -> documentSymbols e'
        CS.Apply _ e1 e2             -> (documentSymbols e1) ++ (documentSymbols e2)
        CS.InfixApply _ e1 _ e2      -> (documentSymbols e1) ++ (documentSymbols e2)
        CS.LeftSection _ e' _        -> documentSymbols e'
        CS.RightSection _ _ e'       -> documentSymbols e'
        CS.Lambda _ _ e'             -> documentSymbols e'
        CS.Let _ decls e             -> (decls >>= documentSymbols) ++ (documentSymbols e) -- TODO: Has 4 arguments in current version of compiler
        CS.Do _ stmts e'             -> (stmts >>= documentSymbols) ++ (documentSymbols e') -- TODO: Has another arg in newer curry-frontend
        CS.IfThenElse _ e1 e2 e3     -> (documentSymbols e1) ++ (documentSymbols e2) ++ (documentSymbols e3)
        CS.Case _ _ e alts           -> (documentSymbols e) ++ (alts >>= documentSymbols) -- TODO: Has another arg in newer curry-frontend
        _                            -> []
        where fieldSymbols (CS.Field _ _ e) = documentSymbols e

instance HasDocumentSymbols (CS.Statement a) where
    documentSymbols stmt = case stmt of
        CS.StmtExpr _ e     -> documentSymbols e
        CS.StmtDecl _ decls -> decls >>= documentSymbols -- TODO: Has three arguments in later curry-base versions
        CS.StmtBind _ _ e   -> documentSymbols e

instance HasDocumentSymbols (CS.Alt a) where
    documentSymbols (CS.Alt _ _ rhs) = documentSymbols rhs

-- Language Server Protocol -> Curry Compiler

-- TODO
