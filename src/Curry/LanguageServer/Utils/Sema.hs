{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
-- | Utilities for extracting semantic information from the AST.
module Curry.LanguageServer.Utils.Sema
    ( HasTypedSpanInfos (..)
    , TypedSpanInfo (..)
    , ModuleAST
    , untypedTopLevelDecls
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Curry.LanguageServer.Utils.Convert (ppToText)
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T

type ModuleAST = CS.Module (Maybe CT.PredType)

-- | Finds top-level function declarations in the module without an explicit type signature.
untypedTopLevelDecls :: CS.Module a -> [(CSPI.SpanInfo, CI.Ident, a)]
untypedTopLevelDecls (CS.Module _ _ _ _ _ _ decls) = untypedDecls
    where typeSigIdents = S.fromList [i | CS.TypeSig _ is _ <- decls, i <- is]
          untypedDecls = [(spi, i, t) | CS.FunctionDecl spi t i _ <- decls, i `S.notMember` typeSigIdents]

data TypedSpanInfo a = TypedSpanInfo T.Text a CSPI.SpanInfo
    deriving (Show, Eq)

class HasTypedSpanInfos e a where
    typedSpanInfos :: e -> [TypedSpanInfo a]

instance HasTypedSpanInfos (CS.Module a) a where
    typedSpanInfos (CS.Module _ _ _ _ _ _ decls) = typedSpanInfos decls

instance HasTypedSpanInfos (CS.Decl a) a where
    typedSpanInfos decl = case decl of
        CS.FunctionDecl _ t i es     -> TypedSpanInfo (ppToText i) t (CSPI.getSpanInfo i) : typedSpanInfos es
        CS.ExternalDecl _ vs         -> typedSpanInfos vs
        CS.PatternDecl _ p rhs       -> typedSpanInfos p ++ typedSpanInfos rhs
        CS.FreeDecl _ vs             -> typedSpanInfos vs
        CS.ClassDecl _ _ _ _ _ _ ds  -> typedSpanInfos ds
        CS.InstanceDecl _ _ _ _ _ ds -> typedSpanInfos ds
        _                            -> []

instance HasTypedSpanInfos (CS.Equation a) a where
    typedSpanInfos (CS.Equation _ _ lhs rhs) = typedSpanInfos lhs ++ typedSpanInfos rhs

instance HasTypedSpanInfos (CS.Var a) a where
    typedSpanInfos (CS.Var t i) = [TypedSpanInfo txt t $ CSPI.getSpanInfo i]
        where txt = ppToText i

instance HasTypedSpanInfos (CS.Pattern a) a where
    typedSpanInfos pat = case pat of
        CS.LiteralPattern spi t _         -> [TypedSpanInfo txt t spi]
        CS.NegativePattern spi t _        -> [TypedSpanInfo txt t spi]
        CS.VariablePattern spi t _        -> [TypedSpanInfo txt t spi]
        CS.ConstructorPattern spi t _ ps  -> TypedSpanInfo txt t spi : typedSpanInfos ps
        CS.InfixPattern spi t p1 _ p2     -> typedSpanInfos p1 ++ typedSpanInfos p2 ++ [TypedSpanInfo txt t spi]
        CS.ParenPattern _ p               -> typedSpanInfos p
        CS.RecordPattern spi t _ fs       -> TypedSpanInfo txt t spi : typedSpanInfos fs
        CS.TuplePattern _ ps              -> typedSpanInfos ps
        CS.ListPattern spi t ps           -> TypedSpanInfo txt t spi : typedSpanInfos ps
        CS.AsPattern _ _ p                -> typedSpanInfos p
        CS.LazyPattern _ p                -> typedSpanInfos p
        CS.FunctionPattern spi t _ ps     -> TypedSpanInfo txt t spi : typedSpanInfos ps
        CS.InfixFuncPattern spi t p1 _ p2 -> typedSpanInfos p1 ++ typedSpanInfos p2 ++ [TypedSpanInfo txt t spi]
        where txt = ppToText pat

instance HasTypedSpanInfos e a => HasTypedSpanInfos (CS.Field e) a where
    typedSpanInfos (CS.Field _ _ e) = typedSpanInfos e

instance HasTypedSpanInfos (CS.Lhs a) a where
    typedSpanInfos lhs = case lhs of
        CS.FunLhs _ _ ps   -> typedSpanInfos ps
        CS.OpLhs _ p1 _ p2 -> typedSpanInfos p1 ++ typedSpanInfos p2
        CS.ApLhs _ l ps    -> typedSpanInfos l ++ typedSpanInfos ps

instance HasTypedSpanInfos (CS.Rhs a) a where
    typedSpanInfos rhs = case rhs of
        CS.SimpleRhs _ _ e ds   -> typedSpanInfos e ++ typedSpanInfos ds
        CS.GuardedRhs _ _ es ds -> typedSpanInfos es ++ typedSpanInfos ds

instance HasTypedSpanInfos (CS.CondExpr a) a where
    typedSpanInfos (CS.CondExpr _ e1 e2) = typedSpanInfos e1 ++ typedSpanInfos e2

instance HasTypedSpanInfos (CS.Expression a) a where
    typedSpanInfos expr = case expr of
        CS.Literal spi t _           -> [TypedSpanInfo txt t spi]
        CS.Variable spi t _          -> [TypedSpanInfo txt t spi]
        CS.Constructor spi t _       -> [TypedSpanInfo txt t spi]
        CS.Paren _ e                 -> typedSpanInfos e
        CS.Typed _ e _               -> typedSpanInfos e
        CS.Record spi t _ fs         -> TypedSpanInfo txt t spi : typedSpanInfos fs
        CS.RecordUpdate _ e fs       -> typedSpanInfos e ++ typedSpanInfos fs
        CS.Tuple _ es                -> typedSpanInfos es
        CS.List spi t es             -> TypedSpanInfo txt t spi : typedSpanInfos es
        CS.ListCompr _ e stmts       -> typedSpanInfos e ++ typedSpanInfos stmts
        CS.EnumFrom _ e              -> typedSpanInfos e
        CS.EnumFromThen _ e1 e2      -> typedSpanInfos e1 ++ typedSpanInfos e2
        CS.EnumFromTo _ e1 e2        -> typedSpanInfos e1 ++ typedSpanInfos e2
        CS.EnumFromThenTo _ e1 e2 e3 -> typedSpanInfos e1 ++ typedSpanInfos e2 ++ typedSpanInfos e3
        CS.UnaryMinus _ e            -> typedSpanInfos e
        CS.Apply _ e1 e2             -> typedSpanInfos e1 ++ typedSpanInfos e2
        CS.InfixApply _ e1 op e2     -> typedSpanInfos e1 ++ typedSpanInfos op ++ typedSpanInfos e2
        CS.LeftSection _ e1 op       -> typedSpanInfos e1 ++ typedSpanInfos op
        CS.RightSection _ op e2      -> typedSpanInfos op ++ typedSpanInfos e2
        CS.Lambda _ ps e             -> typedSpanInfos ps ++ typedSpanInfos e
        CS.Let _ _ ds e              -> typedSpanInfos ds ++ typedSpanInfos e
        CS.Do _ _ stmts e            -> typedSpanInfos stmts ++ typedSpanInfos e
        CS.IfThenElse _ e1 e2 e3     -> typedSpanInfos e1 ++ typedSpanInfos e2 ++ typedSpanInfos e3
        CS.Case _ _ _ e as           -> typedSpanInfos e ++ typedSpanInfos as
        where txt = ppToText expr

instance HasTypedSpanInfos (CS.Alt a) a where
    typedSpanInfos (CS.Alt _ p rhs) = typedSpanInfos p ++ typedSpanInfos rhs

instance HasTypedSpanInfos (CS.InfixOp a) a where
    typedSpanInfos op = case op of
        CS.InfixOp t q     -> [TypedSpanInfo txt t $ CSPI.getSpanInfo q]
        CS.InfixConstr t q -> [TypedSpanInfo txt t $ CSPI.getSpanInfo q]
        where txt = ppToText op

instance HasTypedSpanInfos (CS.Statement a) a where
    typedSpanInfos stmt = case stmt of
        CS.StmtExpr _ e    -> typedSpanInfos e
        CS.StmtDecl _ _ ds -> typedSpanInfos ds
        CS.StmtBind _ p e  -> typedSpanInfos p ++ typedSpanInfos e

instance HasTypedSpanInfos e a => HasTypedSpanInfos [e] a where
    typedSpanInfos = (typedSpanInfos =<<)

instance HasTypedSpanInfos e a => HasTypedSpanInfos (Maybe e) a where
    typedSpanInfos = typedSpanInfos . maybeToList

instance CP.HasPosition (TypedSpanInfo a) where
    getPosition (TypedSpanInfo _ _ spi) = CP.getPosition spi

instance CSPI.HasSpanInfo (TypedSpanInfo a) where
    getSpanInfo (TypedSpanInfo _ _ spi) = spi
    setSpanInfo spi (TypedSpanInfo txt t _) = TypedSpanInfo txt t spi
