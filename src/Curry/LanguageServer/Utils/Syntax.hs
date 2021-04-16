{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-- | AST utilities and typeclasses.
module Curry.LanguageServer.Utils.Syntax
    ( HasExpressions (..)
    , HasDeclarations (..)
    , HasQualIdentifiers (..)
    , HasIdentifiers (..)
    , HasQualIdentifier (..)
    , HasIdentifier (..)
    , elementAt
    , elementsAt
    , elementContains
    , moduleIdentifier
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS

import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Utils.General
import Data.Maybe (maybeToList)
import qualified Language.LSP.Types as J

-- | Fetches the element at the given position.
elementAt :: CSPI.HasSpanInfo e => J.Position -> [e] -> Maybe e
elementAt pos = lastSafe . elementsAt pos

-- | Fetches the elements at the given position.
elementsAt :: CSPI.HasSpanInfo e => J.Position -> [e] -> [e]
elementsAt pos = filter $ elementContains pos

-- | Tests whether the given element in the AST contains the given position.
elementContains :: CSPI.HasSpanInfo e => J.Position -> e -> Bool
elementContains pos = maybe False (rangeElem pos) . currySpanInfo2Range . CSPI.getSpanInfo

-- | Fetches the module identifier for a module.
moduleIdentifier :: CS.Module a -> CI.ModuleIdent
moduleIdentifier (CS.Module _ _ _ ident _ _ _) = ident

class HasExpressions s a | s -> a where
    -- | Fetches all expressions as pre-order traversal
    expressions :: s -> [CS.Expression a]

instance HasExpressions (CS.Module a) a where
    expressions (CS.Module _ _ _ _ _ _ decls) = expressions decls

instance HasExpressions (CS.Decl a) a where
    expressions decl = case decl of
        CS.FunctionDecl _ _ _ eqs    -> expressions eqs
        CS.ClassDecl _ _ _ _ _ ds    -> expressions ds
        CS.InstanceDecl _ _ _ _ _ ds -> expressions ds
        _ -> [] -- TODO

instance HasExpressions (CS.Equation a) a where
    expressions (CS.Equation _ _ rhs) = expressions rhs

instance HasExpressions (CS.Rhs a) a where
    expressions rhs = case rhs of
        CS.SimpleRhs _ _ e decls      -> expressions e ++ expressions decls
        CS.GuardedRhs _ _ conds decls -> expressions conds ++ expressions decls

instance HasExpressions (CS.CondExpr a) a where
    expressions (CS.CondExpr _ e1 e2) = expressions e1 ++ expressions e2

instance HasExpressions (CS.Expression a) a where
    expressions e = e : case e of
        CS.Paren _ e'                -> expressions e'
        CS.Typed _ e' _              -> expressions e'
        CS.Record _ _ _ fields       -> fieldExpressions =<< fields
        CS.RecordUpdate _ e' fields  -> expressions e' ++ (fieldExpressions =<< fields)
        CS.Tuple _ entries           -> expressions entries
        CS.List _ _ entries          -> expressions entries
        CS.ListCompr _ e' stmts      -> expressions e' ++ expressions stmts
        CS.EnumFrom _ e'             -> expressions e'
        CS.EnumFromThen _ e1 e2      -> expressions e1 ++ expressions e2
        CS.EnumFromThenTo _ e1 e2 e3 -> expressions e1 ++ expressions e2 ++ expressions e3
        CS.UnaryMinus _ e'           -> expressions e'
        CS.Apply _ e1 e2             -> expressions e1 ++ expressions e2
        CS.InfixApply _ e1 _ e2      -> expressions e1 ++ expressions e2
        CS.LeftSection _ e' _        -> expressions e'
        CS.RightSection _ _ e'       -> expressions e'
        CS.Lambda _ _ e'             -> expressions e'
        CS.Let _ _ decls e'          -> expressions decls ++ expressions e'
        CS.Do _ _ stmts e'           -> expressions stmts ++ expressions e'
        CS.IfThenElse _ e1 e2 e3     -> expressions e1 ++ expressions e2 ++ expressions e3
        CS.Case _ _ _ e' alts        -> expressions e' ++ expressions alts
        _                            -> []
        where fieldExpressions (CS.Field _ _ e') = expressions e'

instance HasExpressions (CS.Statement a) a where
    expressions stmt = case stmt of
        CS.StmtExpr _ e       -> expressions e
        CS.StmtDecl _ _ decls -> expressions decls
        CS.StmtBind _ _ e     -> expressions e

instance HasExpressions (CS.Alt a) a where
    expressions (CS.Alt _ _ rhs) = expressions rhs

instance HasExpressions s a => HasExpressions [s] a where
    expressions = (expressions =<<)

instance HasExpressions s a => HasExpressions (Maybe s) a where
    expressions = expressions . maybeToList

class HasDeclarations s a | s -> a where
    -- | Fetches all declarations as pre-order traversal
    declarations :: s -> [CS.Decl a]

instance HasDeclarations (CS.Module a) a where
    declarations (CS.Module _ _ _ _ _ _ decls) = declarations decls

instance HasDeclarations (CS.Decl a) a where
    declarations decl = decl : case decl of
        -- TODO: Fetch declarations inside equations/expressions/...
        CS.ClassDecl _ _ _ _ _ ds     -> ds
        CS.InstanceDecl  _ _ _ _ _ ds -> ds
        _                             -> []

instance HasDeclarations s a => HasDeclarations [s] a where
    declarations = (declarations =<<)

instance HasDeclarations s a => HasDeclarations (Maybe s) a where
    declarations = declarations . maybeToList

class HasQualIdentifiers e where
    qualIdentifiers :: e -> [CI.QualIdent]

instance HasQualIdentifiers (CS.Module a) where
    qualIdentifiers (CS.Module _ _ _ _ exports _ decls) = qualIdentifiers exports ++ qualIdentifiers decls

instance HasQualIdentifiers CS.ExportSpec where
    qualIdentifiers (CS.Exporting _ es) = qualIdentifiers es

instance HasQualIdentifiers CS.Export where
    qualIdentifiers export = case export of
        CS.Export _ q           -> [q]
        CS.ExportTypeWith _ q _ -> [q]
        CS.ExportTypeAll _ q    -> [q]
        _                       -> []

instance HasQualIdentifiers (CS.Decl a) where
    qualIdentifiers decl = case decl of
        CS.DataDecl _ _ _ cs qs      -> qualIdentifiers cs ++ qs
        CS.NewtypeDecl _ _ _ c qs    -> qualIdentifiers c ++ qs
        CS.TypeDecl _ _ _ e          -> qualIdentifiers e
        CS.TypeSig _ _ e             -> qualIdentifiers e
        CS.FunctionDecl _ _ _ es     -> qualIdentifiers es
        CS.PatternDecl _ p rhs       -> qualIdentifiers p ++ qualIdentifiers rhs
        CS.DefaultDecl _ es          -> qualIdentifiers es
        CS.ClassDecl _ _ _ _ _ ds    -> qualIdentifiers ds
        CS.InstanceDecl _ _ _ q _ ds -> q : qualIdentifiers ds
        _                            -> []

instance HasQualIdentifiers CS.ConstrDecl where
    qualIdentifiers cdecl = case cdecl of
        CS.ConstrDecl _ _ ts   -> qualIdentifiers ts
        CS.ConOpDecl _ t1 _ t2 -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.RecordDecl _ _ fs   -> qualIdentifiers fs

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
        CS.FunLhs _ _ ps   -> qualIdentifiers ps
        CS.OpLhs _ p1 _ p2 -> qualIdentifiers p1 ++ qualIdentifiers p2
        CS.ApLhs _ l ps    -> qualIdentifiers l ++ qualIdentifiers ps

instance HasQualIdentifiers (CS.Pattern a) where
    qualIdentifiers pat = case pat of
        CS.ConstructorPattern _ _ q ps  -> q : qualIdentifiers ps
        CS.InfixPattern _ _ p1 q p2     -> q : qualIdentifiers p1 ++ qualIdentifiers p2
        CS.ParenPattern _ p             -> qualIdentifiers p
        CS.RecordPattern _ _ q fs       -> q : qualIdentifiers fs
        CS.TuplePattern _ ps            -> qualIdentifiers ps
        CS.ListPattern _ _ ps           -> qualIdentifiers ps
        CS.AsPattern _ _ p              -> qualIdentifiers p
        CS.LazyPattern _ p              -> qualIdentifiers p
        CS.FunctionPattern _ _ q ps     -> q : qualIdentifiers ps
        CS.InfixFuncPattern _ _ p1 q p2 -> q : qualIdentifiers p1 ++ qualIdentifiers p2
        _                               -> []

instance HasQualIdentifiers (CS.Rhs a) where
    qualIdentifiers rhs = case rhs of
        CS.SimpleRhs _ _ e ds   -> qualIdentifiers e ++ qualIdentifiers ds
        CS.GuardedRhs _ _ es ds -> qualIdentifiers es ++ qualIdentifiers ds

instance HasQualIdentifiers (CS.CondExpr a) where
    qualIdentifiers (CS.CondExpr _ e1 e2) = qualIdentifiers e1 ++ qualIdentifiers e2

instance HasQualIdentifiers (CS.Expression a) where
    qualIdentifiers e = case e of
        CS.Paren _ e'                -> qualIdentifiers e'
        CS.Typed _ e' t              -> qualIdentifiers e' ++ qualIdentifiers t
        CS.Record _ _ q fields       -> q : qualIdentifiers fields
        CS.RecordUpdate _ e' fields  -> qualIdentifiers e' ++ qualIdentifiers fields
        CS.Tuple _ entries           -> qualIdentifiers entries
        CS.List _ _ entries          -> qualIdentifiers entries
        CS.ListCompr _ e' stmts      -> qualIdentifiers e' ++ qualIdentifiers stmts
        CS.EnumFrom _ e'             -> qualIdentifiers e'
        CS.EnumFromThen _ e1 e2      -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.EnumFromThenTo _ e1 e2 e3 -> qualIdentifiers e1 ++ qualIdentifiers e2 ++ qualIdentifiers e3
        CS.UnaryMinus _ e'           -> qualIdentifiers e'
        CS.Apply _ e1 e2             -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.InfixApply _ e1 _ e2      -> qualIdentifiers e1 ++ qualIdentifiers e2
        CS.LeftSection _ e' _        -> qualIdentifiers e'
        CS.RightSection _ _ e'       -> qualIdentifiers e'
        CS.Lambda _ _ e'             -> qualIdentifiers e'
        CS.Let _ _ decls e'          -> qualIdentifiers decls ++ qualIdentifiers e'
        CS.Do _ _ stmts e'           -> qualIdentifiers stmts ++ qualIdentifiers e'
        CS.IfThenElse _ e1 e2 e3     -> qualIdentifiers e1 ++ qualIdentifiers e2 ++ qualIdentifiers e3
        CS.Case _ _ _ e' alts        -> qualIdentifiers e' ++ qualIdentifiers alts
        CS.Variable _ _ q            -> [q]
        CS.Constructor _ _ q         -> [q]
        _                            -> []

instance HasQualIdentifiers a => HasQualIdentifiers (CS.Field a) where
    qualIdentifiers (CS.Field _ q e) = q : qualIdentifiers e

instance HasQualIdentifiers (CS.Statement a) where
    qualIdentifiers stmt = case stmt of
        CS.StmtExpr _ e    -> qualIdentifiers e
        CS.StmtDecl _ _ ds -> qualIdentifiers ds
        CS.StmtBind _ p e  -> qualIdentifiers p ++ qualIdentifiers e

instance HasQualIdentifiers (CS.Alt a) where
    qualIdentifiers (CS.Alt _ p rhs) = qualIdentifiers p ++ qualIdentifiers rhs

instance HasQualIdentifiers CS.TypeExpr where
    qualIdentifiers texpr = case texpr of
        CS.ConstructorType _ q -> [q]
        CS.ApplyType _ t1 t2   -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.TupleType _ ts      -> qualIdentifiers ts
        CS.ListType _ t        -> qualIdentifiers t
        CS.ArrowType _ t1 t2   -> qualIdentifiers t1 ++ qualIdentifiers t2
        CS.ParenType _ t       -> qualIdentifiers t
        CS.ForallType _ _ t    -> qualIdentifiers t
        _                      -> []

instance HasQualIdentifiers CS.QualTypeExpr where
    qualIdentifiers (CS.QualTypeExpr _ ctx t) = qualIdentifiers ctx ++ qualIdentifiers t

instance HasQualIdentifiers CS.Constraint where
    qualIdentifiers (CS.Constraint _ q t) = q : qualIdentifiers t

instance HasQualIdentifiers a => HasQualIdentifiers [a] where
    qualIdentifiers = (qualIdentifiers =<<)

instance HasQualIdentifiers a => HasQualIdentifiers (Maybe a) where
    qualIdentifiers = qualIdentifiers . maybeToList

class HasIdentifiers e where
    identifiers :: e -> [CI.Ident]

instance HasIdentifiers (CS.Module a) where
    identifiers (CS.Module _ _ _ _ _ imps decls) = identifiers imps ++ identifiers decls

instance HasIdentifiers CS.ImportDecl where
    identifiers (CS.ImportDecl _ _ _ _ spec) = identifiers spec

instance HasIdentifiers CS.ImportSpec where
    identifiers spec = case spec of
        CS.Importing _ is -> identifiers is
        CS.Hiding _ is    -> identifiers is

instance HasIdentifiers CS.Import where
    identifiers imp = case imp of
        CS.Import _ i            -> [i]
        CS.ImportTypeWith _ i is -> i : is
        CS.ImportTypeAll _ i     -> [i]

instance HasIdentifiers (CS.Decl a) where
    identifiers decl = case decl of
        CS.InfixDecl _ _ _ is         -> is
        CS.DataDecl _ i is cdecls _   -> (i : is) ++ identifiers cdecls
        CS.ExternalDataDecl _ i is    -> i : is
        CS.NewtypeDecl _ i is cdecl _ -> (i : is) ++ identifiers cdecl
        CS.TypeDecl _ i is t          -> (i : is) ++ identifiers t
        CS.TypeSig _ is t             -> is ++ identifiers t
        CS.FunctionDecl _ _ i es      -> i : identifiers es
        CS.ExternalDecl _ vs          -> identifiers vs
        CS.PatternDecl _ p rhs        -> identifiers p ++ identifiers rhs
        CS.FreeDecl _ vs              -> identifiers vs
        CS.DefaultDecl _ ts           -> identifiers ts
        CS.ClassDecl _ _ c i1 i2 ds   -> identifiers c ++ i1 : i2 : identifiers ds
        CS.InstanceDecl _ _ c _ _ ds  -> identifiers c ++ identifiers ds

instance HasIdentifiers CS.Constraint where
    identifiers (CS.Constraint _ _ t) = identifiers t

instance HasIdentifiers (CS.Equation a) where
    identifiers (CS.Equation _ lhs rhs) = identifiers lhs ++ identifiers rhs

instance HasIdentifiers (CS.Pattern a) where
    identifiers pat = case pat of
        CS.VariablePattern _ _ i        -> [i]
        CS.ConstructorPattern _ _ _ ps  -> identifiers ps
        CS.InfixPattern _ _ p1 _ p2     -> identifiers p1 ++ identifiers p2
        CS.ParenPattern _ p             -> identifiers p
        CS.RecordPattern _ _ _ fs       -> identifiers fs
        CS.TuplePattern _ ps            -> identifiers ps
        CS.ListPattern _ _ ps           -> identifiers ps
        CS.AsPattern _ i p              -> i : identifiers p
        CS.LazyPattern _ p              -> identifiers p
        CS.FunctionPattern _ _ _ ps     -> identifiers ps
        CS.InfixFuncPattern _ _ p1 _ p2 -> identifiers p1 ++ identifiers p2
        _                               -> []

instance HasIdentifiers (CS.Statement a) where
    identifiers stmt = case stmt of
        CS.StmtExpr _ e    -> identifiers e
        CS.StmtDecl _ _ ds -> identifiers ds
        CS.StmtBind _ p e  -> identifiers p ++ identifiers e

instance HasIdentifiers (CS.Lhs a) where
    identifiers lhs = case lhs of
        CS.FunLhs _ _ ps   -> identifiers ps
        CS.OpLhs _ p1 _ p2 -> identifiers p1 ++ identifiers p2
        CS.ApLhs _ l ps    -> identifiers l ++ identifiers ps

instance HasIdentifiers (CS.Rhs a) where
    identifiers rhs = case rhs of
        CS.SimpleRhs _ _ e ds   -> identifiers e ++ identifiers ds
        CS.GuardedRhs _ _ es ds -> identifiers es ++ identifiers ds

instance HasIdentifiers (CS.CondExpr a) where
    identifiers (CS.CondExpr _ e1 e2) = identifiers e1 ++ identifiers e2

instance HasIdentifiers (CS.Expression a) where
    identifiers e = case e of
        CS.Paren _ e'                -> identifiers e'
        CS.Typed _ e' _              -> identifiers e'
        CS.Record _ _ _ fields       -> identifiers fields
        CS.RecordUpdate _ e' fields  -> identifiers e' ++ identifiers fields
        CS.Tuple _ entries           -> identifiers entries
        CS.List _ _ entries          -> identifiers entries
        CS.ListCompr _ e' stmts      -> identifiers e' ++ identifiers stmts
        CS.EnumFrom _ e'             -> identifiers e'
        CS.EnumFromThen _ e1 e2      -> identifiers e1 ++ identifiers e2
        CS.EnumFromThenTo _ e1 e2 e3 -> identifiers e1 ++ identifiers e2 ++ identifiers e3
        CS.UnaryMinus _ e'           -> identifiers e'
        CS.Apply _ e1 e2             -> identifiers e1 ++ identifiers e2
        CS.InfixApply _ e1 _ e2      -> identifiers e1 ++ identifiers e2
        CS.LeftSection _ e' _        -> identifiers e'
        CS.RightSection _ _ e'       -> identifiers e'
        CS.Lambda _ _ e'             -> identifiers e'
        CS.Let _ _ decls e'          -> identifiers decls ++ identifiers e'
        CS.Do _ _ stmts e'           -> identifiers stmts ++ identifiers e'
        CS.IfThenElse _ e1 e2 e3     -> identifiers e1 ++ identifiers e2 ++ identifiers e3
        CS.Case _ _ _ e' alts        -> identifiers e' ++ identifiers alts
        _                            -> []

instance HasIdentifiers (CS.Alt a) where
    identifiers (CS.Alt _ p rhs) = identifiers p ++ identifiers rhs

instance HasIdentifiers a => HasIdentifiers (CS.Field a) where
    identifiers (CS.Field _ _ e) = identifiers e

instance HasIdentifiers (CS.Var a) where
    identifiers (CS.Var _ i) = [i]

instance HasIdentifiers CS.ConstrDecl where
    identifiers cdecl = case cdecl of
        CS.ConstrDecl _ i ts   -> i : identifiers ts
        CS.ConOpDecl _ t1 i t2 -> i : (identifiers t1 ++ identifiers t2)
        CS.RecordDecl _ i fs   -> i : identifiers fs

instance HasIdentifiers CS.FieldDecl where
    identifiers (CS.FieldDecl _ _ t) = identifiers t

instance HasIdentifiers CS.NewConstrDecl where
    identifiers cdecl = case cdecl of
        CS.NewConstrDecl _ i t       -> i : identifiers t
        CS.NewRecordDecl _ i (i', t) -> i : i' : identifiers t

instance HasIdentifiers CS.TypeExpr where
    identifiers texpr = case texpr of
        CS.ApplyType _ t1 t2 -> identifiers t1 ++ identifiers t2
        CS.VariableType _ i  -> [i]
        CS.TupleType _ ts    -> identifiers ts
        CS.ListType _ t      -> identifiers t
        CS.ArrowType _ t1 t2 -> identifiers t1 ++ identifiers t2
        CS.ParenType _ t     -> identifiers t
        CS.ForallType _ is t -> is ++ identifiers t
        _                    -> []

instance HasIdentifiers CS.QualTypeExpr where
    identifiers (CS.QualTypeExpr _ c t) = identifiers c ++ identifiers t

instance HasIdentifiers a => HasIdentifiers [a] where
    identifiers = (identifiers =<<)

instance HasIdentifiers a => HasIdentifiers (Maybe a) where
    identifiers = identifiers . maybeToList

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
