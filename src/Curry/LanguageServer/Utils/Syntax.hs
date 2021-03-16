-- | AST utilities and typeclasses.
module Curry.LanguageServer.Utils.Syntax (
    HasExpressions (..),
    HasDeclarations (..),
    HasQualIdentifiers (..),
    HasIdentifiers (..),
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
import qualified Language.LSP.Types as J

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
        CS.FunctionDecl _ _ _ eqs    -> eqs >>= expressions
        CS.ClassDecl _ _ _ _ _ ds    -> ds >>= expressions
        CS.InstanceDecl _ _ _ _ _ ds -> ds >>= expressions
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
        CS.Case _ _ _ e' alts        -> expressions e' ++ (alts >>= expressions)
        _                            -> []
        where fieldExpressions (CS.Field _ _ e') = expressions e'

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
    declarations (CS.Module _ _ _ _ _ _ decls) = decls >>= declarations

instance HasDeclarations CS.Decl where
    declarations decl = decl : case decl of
        -- TODO: Fetch declarations inside equations/expressions/...
        CS.ClassDecl _ _ _ _ _ ds     -> ds
        CS.InstanceDecl  _ _ _ _ _ ds -> ds
        _                        -> []

class HasQualIdentifiers e where
    qualIdentifiers :: e -> [CI.QualIdent]

instance HasQualIdentifiers (CS.Module a) where
    qualIdentifiers (CS.Module _ _ _ _ _ _ decls) = decls >>= qualIdentifiers

instance HasQualIdentifiers (CS.Decl a) where
    qualIdentifiers decl = case decl of
        CS.DataDecl _ _ _ cs qs      -> (cs >>= qualIdentifiers) ++ qs
        CS.NewtypeDecl _ _ _ c qs    -> qualIdentifiers c ++ qs
        CS.TypeDecl _ _ _ e          -> qualIdentifiers e
        CS.TypeSig _ _ e             -> qualIdentifiers e
        CS.FunctionDecl _ _ _ es     -> es >>= qualIdentifiers
        CS.PatternDecl _ p rhs       -> qualIdentifiers p ++ qualIdentifiers rhs
        CS.DefaultDecl _ es          -> es >>= qualIdentifiers
        CS.ClassDecl _ _ _ _ _ ds    -> ds >>= qualIdentifiers
        CS.InstanceDecl _ _ _ q _ ds -> q : (ds >>= qualIdentifiers)
        _                            -> []

instance HasQualIdentifiers CS.ConstrDecl where
    qualIdentifiers cdecl = case cdecl of
        CS.ConstrDecl _ _ ts   -> ts >>= qualIdentifiers
        CS.ConOpDecl _ t1 _ t2 -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.RecordDecl _ _ fs -> fs >>= qualIdentifiers

instance HasQualIdentifiers CS.FieldDecl where
    qualIdentifiers (CS.FieldDecl _ _ t) = qualIdentifiers t

instance HasQualIdentifiers CS.NewConstrDecl where
    qualIdentifiers cdecl = case cdecl of
        CS.NewConstrDecl _ _ t      -> qualIdentifiers t
        CS.NewRecordDecl _ _ (_, t) -> qualIdentifiers t

instance HasQualIdentifiers (CS.Equation a) where
    qualIdentifiers (CS.Equation _ lhs rhs) = qualIdentifiers lhs ++ qualIdentifiers rhs

instance HasQualIdentifiers (CS.Lhs a) where
    qualIdentifiers lhs = case lhs of
        CS.FunLhs _ _ ps   -> ps >>= qualIdentifiers
        CS.OpLhs _ p1 _ p2 -> qualIdentifiers p1 ++ qualIdentifiers p2
        CS.ApLhs _ l ps    -> qualIdentifiers l ++ (ps >>= qualIdentifiers)

instance HasQualIdentifiers (CS.Pattern a) where
    qualIdentifiers pat = case pat of
        CS.ConstructorPattern _ _ q ps  -> q : (ps >>= qualIdentifiers)
        CS.InfixPattern _ _ p1 q p2     -> q : qualIdentifiers p1 ++ qualIdentifiers p2
        CS.ParenPattern _ p             -> qualIdentifiers p
        CS.RecordPattern _ _ q fs       -> q : (fs >>= qualIdentifiers)
        CS.TuplePattern _ ps            -> ps >>= qualIdentifiers
        CS.ListPattern _ _ ps           -> ps >>= qualIdentifiers
        CS.AsPattern _ _ p              -> qualIdentifiers p
        CS.LazyPattern _ p              -> qualIdentifiers p
        CS.FunctionPattern _ _ q ps     -> q : (ps >>= qualIdentifiers)
        CS.InfixFuncPattern _ _ p1 q p2 -> q : qualIdentifiers p1 ++ qualIdentifiers p2
        _                               -> []

instance HasQualIdentifiers (CS.Rhs a) where
    qualIdentifiers rhs = case rhs of
        CS.SimpleRhs _ _ e ds   -> qualIdentifiers e ++ (ds >>= qualIdentifiers)
        CS.GuardedRhs _ _ es ds -> (es >>= qualIdentifiers) ++ (ds >>= qualIdentifiers)

instance HasQualIdentifiers (CS.CondExpr a) where
    qualIdentifiers (CS.CondExpr _ e1 e2) = qualIdentifiers e1 ++ qualIdentifiers e2

instance HasQualIdentifiers (CS.Expression a) where
    qualIdentifiers e = case e of
        CS.Paren _ e'                -> qualIdentifiers e'
        CS.Typed _ e' _              -> qualIdentifiers e'
        CS.Record _ _ q fields       -> q : (fields >>= qualIdentifiers)
        CS.RecordUpdate _ e' fields  -> qualIdentifiers e' ++ (fields >>= qualIdentifiers)
        CS.Tuple _ entries           -> entries >>= qualIdentifiers
        CS.List _ _ entries          -> entries >>= qualIdentifiers
        CS.ListCompr _ e' stmts      -> qualIdentifiers e' ++ (stmts >>= qualIdentifiers)
        CS.EnumFrom _ e'             -> qualIdentifiers e'
        CS.EnumFromThen _ e1 e2      -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.EnumFromThenTo _ e1 e2 e3 -> qualIdentifiers e1 ++ qualIdentifiers e2 ++ qualIdentifiers e3
        CS.UnaryMinus _ e'           -> qualIdentifiers e'
        CS.Apply _ e1 e2             -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.InfixApply _ e1 _ e2      -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.LeftSection _ e' _        -> qualIdentifiers e'
        CS.RightSection _ _ e'       -> qualIdentifiers e'
        CS.Lambda _ _ e'             -> qualIdentifiers e'
        CS.Let _ _ decls e'          -> (decls >>= qualIdentifiers) ++ qualIdentifiers e'
        CS.Do _ _ stmts e'           -> (stmts >>= qualIdentifiers) ++ qualIdentifiers e'
        CS.IfThenElse _ e1 e2 e3     -> qualIdentifiers e1 ++ qualIdentifiers e2 ++ qualIdentifiers e3
        CS.Case _ _ _ e' alts        -> qualIdentifiers e' ++ (alts >>= qualIdentifiers)
        CS.Variable _ _ q            -> [q]
        CS.Constructor _ _ q         -> [q]
        _                            -> []

instance HasQualIdentifiers a => HasQualIdentifiers (CS.Field a) where
    qualIdentifiers (CS.Field _ _ e) = qualIdentifiers e

instance HasQualIdentifiers (CS.Statement a) where
    qualIdentifiers stmt = case stmt of
        CS.StmtExpr _ e    -> qualIdentifiers e
        CS.StmtDecl _ _ ds -> ds >>= qualIdentifiers
        CS.StmtBind _ p e  -> qualIdentifiers p ++ qualIdentifiers e

instance HasQualIdentifiers (CS.Alt a) where
    qualIdentifiers (CS.Alt _ p rhs) = qualIdentifiers p ++ qualIdentifiers rhs

instance HasQualIdentifiers CS.TypeExpr where
    qualIdentifiers texpr = case texpr of
        CS.ConstructorType _ q -> [q]
        CS.ApplyType _ t1 t2   -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.TupleType _ ts      -> ts >>= qualIdentifiers
        CS.ListType _ t        -> qualIdentifiers t
        CS.ArrowType _ t1 t2   -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.ParenType _ t       -> qualIdentifiers t
        CS.ForallType _ _ t    -> qualIdentifiers t
        _                      -> []

instance HasQualIdentifiers CS.QualTypeExpr where
    qualIdentifiers (CS.QualTypeExpr _ _ t) = qualIdentifiers t

class HasIdentifiers e where
    identifiers :: e -> [CI.Ident]

instance HasIdentifiers (CS.Module a) where
    identifiers (CS.Module _ _ _ _ _ _ decls) = decls >>= identifiers

instance HasIdentifiers (CS.Decl a) where
    identifiers = undefined
    -- identifiers decl = case decl of
    --     CS.InfixDecl _ _ _ is         -> is
    --     CS.DataDecl _ i is cdecls _   -> (i : is) ++ cdecls >>= identifiers
    --     CS.ExternalDataDecl _ i is    -> i : is
    --     CS.NewtypeDecl _ i is cdecl _ -> (i : is) ++ identifiers cdecl
    --     CS.TypeDecl _ i is t          -> (i : is) ++ identifiers t

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
