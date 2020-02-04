module Curry.LanguageServer.Utils.Syntax (HasExpressions (..), expressionAt) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Curry.Base.SpanInfo as CSPI

import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import qualified Language.Haskell.LSP.Types as J

-- | Fetches the expression in the AST at the given position.
expressionAt :: HasExpressions s => J.Position -> s a -> Maybe (CS.Expression a)
expressionAt pos = lastSafe . filter (elementContains pos) . expressions

-- | Tests whether the given element in the AST contains the given position.
elementContains :: CSPI.HasSpanInfo e => J.Position -> e -> Bool
elementContains pos = (maybe False (rangeElem pos)) . currySpanInfo2Range . CSPI.getSpanInfo

class HasExpressions s where
    -- | Fetches all expressions as pre-order traversal
    expressions :: s a -> [CS.Expression a]

instance HasExpressions CS.Module where
    expressions (CS.Module _ _ _ _ _ decls) = decls >>= expressions -- TODO: Has seven arguments in later curry-base versions

instance HasExpressions CS.Decl where
    expressions decl = case decl of
        CS.FunctionDecl _ _ _ eqs -> eqs >>= expressions
        _ -> [] -- TODO

instance HasExpressions CS.Equation where
    expressions (CS.Equation _ _ rhs) = expressions rhs

instance HasExpressions CS.Rhs where
    expressions rhs = case rhs of
        CS.SimpleRhs _ e decls      -> (expressions e) ++ (decls >>= expressions) -- TODO: Has four arguments in later curry-base versions
        CS.GuardedRhs _ conds decls -> (conds >>= expressions) ++ (decls >>= expressions)  -- TODO: Has four arguments in later curry-base versions

instance HasExpressions CS.CondExpr where
    expressions (CS.CondExpr _ e1 e2) = (expressions e1) ++ (expressions e2)

instance HasExpressions CS.Expression where
    expressions e = e : case e of
        CS.Paren _ e'               -> expressions e'
        CS.Typed _ e' _             -> expressions e'
        CS.Record _ _ _ fields      -> fields >>= fieldExpressions
        CS.RecordUpdate _ e' fields -> (expressions e') ++ (fields >>= fieldExpressions)
        CS.Tuple _ entries          -> entries >>= expressions
        CS.List _ _ entries         -> entries >>= expressions
        CS.ListCompr _ e stmts      -> (expressions e) ++ (stmts >>= expressions)
        _ -> [] -- TODO
        where fieldExpressions (CS.Field _ _ e) = expressions e

instance HasExpressions CS.Statement where
    expressions stmt = case stmt of
        CS.StmtExpr _ e     -> expressions e
        CS.StmtDecl _ decls -> decls >>= expressions -- TODO: Has three arguments in later curry-base versions
        CS.StmtBind _ _ e   -> expressions e
