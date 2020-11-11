-- | AST utilities and typeclasses.
module Curry.LanguageServer.Utils.Syntax (
    HasExpressions (..),
    HasDeclarations (..),
    HasQualIdentifier (..),
    HasIdentifier (..),
    ModuleAST,
    elementAt,
    moduleIdentifier
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.Types as CT
import qualified Curry.Syntax as CS

import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import qualified Language.Haskell.LSP.Types as J

type ModuleAST = CS.Module CT.PredType

-- | Fetches the element at the given position.
elementAt :: CSPI.HasSpanInfo e => J.Position -> [e] -> Maybe e
elementAt pos = lastSafe . filter (elementContains pos)

-- | Tests whether the given element in the AST contains the given position.
elementContains :: CSPI.HasSpanInfo e => J.Position -> e -> Bool
elementContains pos = maybe False (rangeElem pos) . currySpanInfo2Range . CSPI.getSpanInfo

-- | Fetches the module identifier for a module.
moduleIdentifier :: CS.Module a -> CI.ModuleIdent
moduleIdentifier (CS.Module _ _ _ ident _ _ _) = ident

class HasExpressions s where
    -- | Fetches all expressions as pre-order traversal
    expressions :: s a -> [CS.Expression a]

instance HasExpressions CS.Module where
    expressions (CS.Module _ _ _ _ _ _ decls) = decls >>= expressions

instance HasExpressions CS.Decl where
    expressions decl = case decl of
        CS.FunctionDecl _ _ _ eqs -> eqs >>= expressions
        _ -> [] -- TODO

instance HasExpressions CS.Equation where
    expressions (CS.Equation _ _ rhs) = expressions rhs

instance HasExpressions CS.Rhs where
    expressions rhs = case rhs of
        CS.SimpleRhs _ _ e decls      -> expressions e ++ (decls >>= expressions)
        CS.GuardedRhs _ _ conds decls -> (conds >>= expressions) ++ (decls >>= expressions)

instance HasExpressions CS.CondExpr where
    expressions (CS.CondExpr _ e1 e2) = expressions e1 ++ expressions e2

instance HasExpressions CS.Expression where
    expressions e = e : case e of
        CS.Paren _ e'                -> expressions e'
        CS.Typed _ e' _              -> expressions e'
        CS.Record _ _ _ fields       -> fields >>= fieldExpressions
        CS.RecordUpdate _ e' fields  -> expressions e' ++ (fields >>= fieldExpressions)
        CS.Tuple _ entries           -> entries >>= expressions
        CS.List _ _ entries          -> entries >>= expressions
        CS.ListCompr _ e' stmts      -> expressions e' ++ (stmts >>= expressions)
        CS.EnumFrom _ e'             -> expressions e'
        CS.EnumFromThen _ e1 e2      -> expressions e1 ++ expressions e2
        CS.EnumFromThenTo _ e1 e2 e3 -> expressions e1 ++ expressions e2 ++ expressions e3
        CS.UnaryMinus _ e'           -> expressions e'
        CS.Apply _ e1 e2             -> expressions e1 ++ expressions e2
        CS.InfixApply _ e1 _ e2      -> expressions e1 ++ expressions e2
        CS.LeftSection _ e' _        -> expressions e'
        CS.RightSection _ _ e'       -> expressions e'
        CS.Lambda _ _ e'             -> expressions e'
        CS.Let _ _ decls e'          -> (decls >>= expressions) ++ expressions e'
        CS.Do _ _ stmts e'           -> (stmts >>= expressions) ++ expressions e'
        CS.IfThenElse _ e1 e2 e3     -> expressions e1 ++ expressions e2 ++ expressions e3
        CS.Case _ _ _ e alts         -> expressions e ++ (alts >>= expressions)
        _                            -> []
        where fieldExpressions (CS.Field _ _ e) = expressions e

instance HasExpressions CS.Statement where
    expressions stmt = case stmt of
        CS.StmtExpr _ e       -> expressions e
        CS.StmtDecl _ _ decls -> decls >>= expressions
        CS.StmtBind _ _ e     -> expressions e

instance HasExpressions CS.Alt where
    expressions (CS.Alt _ _ rhs) = expressions rhs

class HasDeclarations s where
    -- | Fetches all declarations as pre-order traversal
    declarations :: s a -> [CS.Decl a]

instance HasDeclarations CS.Module where
    declarations (CS.Module _ _ _ _ _ _ decls) = declarations =<< decls

instance HasDeclarations CS.Decl where
    declarations decl = decl : case decl of
        -- TODO: Fetch declarations inside equations/expressions/...
        CS.ClassDecl _ _ _ _ _ ds     -> ds
        CS.InstanceDecl  _ _ _ _ _ ds -> ds
        _                        -> []

class HasQualIdentifier e where
    qualIdentifier :: e -> Maybe CI.QualIdent

instance HasQualIdentifier (CS.Expression a) where
    qualIdentifier e = case e of
        CS.Variable _ _ ident    -> Just ident
        CS.Constructor _ _ ident -> Just ident
        CS.Record _ _ ident _    -> Just ident
        _                        -> Nothing

instance HasQualIdentifier (CS.Pattern a) where
    qualIdentifier e = case e of
        CS.ConstructorPattern _ _ ident _ -> Just ident
        CS.InfixPattern _ _ _ ident _     -> Just ident
        CS.RecordPattern _ _ ident _      -> Just ident
        CS.FunctionPattern _ _ ident _    -> Just ident
        _                                 -> Nothing

class HasIdentifier e where
    identifier :: e -> Maybe CI.Ident

instance HasIdentifier (CS.Decl a) where
    identifier decl = case decl of
        CS.DataDecl _ ident _ _ _     -> Just ident
        CS.ExternalDataDecl _ ident _ -> Just ident
        CS.NewtypeDecl _ ident _ _ _  -> Just ident
        CS.TypeDecl _ ident _ _       -> Just ident
        CS.FunctionDecl _ _ ident _   -> Just ident
        CS.ClassDecl _ _ _ ident _ _  -> Just ident
        _                             -> Nothing
