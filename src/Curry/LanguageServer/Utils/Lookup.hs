{-# LANGUAGE FlexibleInstances #-}
-- | Position lookup in the AST.
module Curry.LanguageServer.Utils.Lookup (
    findQualIdentAtPos,
    findTypeAtPos,
    HasIdentifiersInScope (..)
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Applicative
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range, ppToText)
import Curry.LanguageServer.Utils.General (rangeElem, Insertable (..), ConstMap (..), joinFst, (<.$>))
import Curry.LanguageServer.Utils.Syntax
import Curry.LanguageServer.Utils.Sema
import Data.Foldable (fold)
import qualified Data.Text as T
import qualified Language.LSP.Types as J

-- | Finds identifier and (occurrence) span info at a given position.
findQualIdentAtPos :: ModuleAST -> J.Position -> Maybe (CI.QualIdent, CSPI.SpanInfo)
findQualIdentAtPos ast pos = qualIdent <|> exprIdent <|> basicIdent
    where qualIdent = withSpanInfo <$> elementAt pos (qualIdentifiers ast)
          exprIdent = joinFst $ qualIdentifier <.$> withSpanInfo <$> elementAt pos (expressions ast)
          basicIdent = CI.qualify <.$> withSpanInfo <$> elementAt pos (identifiers ast)



-- | Finds the type at the given position.
findTypeAtPos :: ModuleAST -> J.Position -> Maybe (TypedSpanInfo CT.PredType)
findTypeAtPos ast pos = elementAt pos $ typedSpanInfos ast

withSpanInfo :: CSPI.HasSpanInfo a => a -> (a, CSPI.SpanInfo)
withSpanInfo x = (x, CSPI.getSpanInfo x)

withName :: CI.Ident -> (T.Text, CI.Ident)
withName i = (ppToText i, i)

containsPos :: CSPI.HasSpanInfo a => a -> J.Position -> Bool
containsPos x pos = maybe False (rangeElem pos) $ currySpanInfo2Range x

class HasIdentifiersInScope a where
    -- | Finds all accessible identifiers at the given position, using the innermost shadowed one.
    identifiersInScope :: J.Position -> a -> ConstMap T.Text CI.Ident

instance HasIdentifiersInScope (CS.Module a) where
    identifiersInScope pos (CS.Module _ _ _ _ _ _ decls) = identifiersInScope pos decls

instance HasIdentifiersInScope (CS.Decl a) where
    identifiersInScope pos decl
        | decl `containsPos` pos = case decl of
            CS.DataDecl _ _ vs _ _    -> insertAll (withName <$> vs) mempty
            CS.NewtypeDecl _ _ vs _ _ -> insertAll (withName <$> vs) mempty
            CS.TypeDecl _ _ vs _      -> insertAll (withName <$> vs) mempty
            CS.FunctionDecl _ _ i eqs -> insert (withName i) $ identifiersInScope pos eqs
            CS.PatternDecl _ p rhs    -> insertAll (withName <$> identifiers p) $ identifiersInScope pos rhs
            _                         -> mempty -- TODO: Class/instance decls
        | otherwise              = mempty

instance HasIdentifiersInScope (CS.Equation a) where
    identifiersInScope pos eqn@(CS.Equation _ lhs rhs)
        | eqn `containsPos` pos = insertAll (withName <$> identifiers lhs) $ identifiersInScope pos rhs
        | otherwise             = mempty

instance HasIdentifiersInScope (CS.Rhs a) where
    identifiersInScope pos rhs
        | rhs `containsPos` pos = case rhs of
            CS.SimpleRhs _ _ e ds   -> identifiersInScope pos e <> identifiersInScope pos ds
            CS.GuardedRhs _ _ cs ds -> identifiersInScope pos cs <> identifiersInScope pos ds
        | otherwise             = mempty

instance HasIdentifiersInScope (CS.CondExpr a) where
    identifiersInScope pos c@(CS.CondExpr _ e1 e2)
        | c `containsPos` pos = identifiersInScope pos e1 <> identifiersInScope pos e2
        | otherwise           = mempty

instance HasIdentifiersInScope (CS.Expression a) where
    identifiersInScope pos expr
        | expr `containsPos` pos = case expr of
            CS.Paren _ e                 -> identifiersInScope pos e
            CS.Typed _ e _               -> identifiersInScope pos e
            CS.Record _ _ _ fs           -> identifiersInScope pos fs
            CS.RecordUpdate _ e fs       -> identifiersInScope pos e <> identifiersInScope pos fs
            CS.Tuple _ es                -> identifiersInScope pos es
            CS.List _ _ es               -> identifiersInScope pos es
            CS.ListCompr _ e stmts       -> identifiersInScope pos e <> identifiersInScope pos stmts
            CS.EnumFrom _ e              -> identifiersInScope pos e
            CS.EnumFromThen _ e1 e2      -> identifiersInScope pos e1 <> identifiersInScope pos e2
            CS.EnumFromTo _ e1 e2        -> identifiersInScope pos e1 <> identifiersInScope pos e2
            CS.EnumFromThenTo _ e1 e2 e3 -> identifiersInScope pos e1 <> identifiersInScope pos e2 <> identifiersInScope pos e3
            CS.UnaryMinus _ e            -> identifiersInScope pos e
            CS.Apply _ e1 e2             -> identifiersInScope pos e1 <> identifiersInScope pos e2
            CS.InfixApply _ e1 _ e2      -> identifiersInScope pos e1 <> identifiersInScope pos e2
            CS.LeftSection _ e _         -> identifiersInScope pos e
            CS.RightSection _ _ e        -> identifiersInScope pos e
            CS.Lambda _ ps e             -> insertAll (withName <$> identifiers ps) $ identifiersInScope pos e
            CS.Let _ _ ds e              -> identifiersInScope pos ds <> identifiersInScope pos e
            CS.Do _ _ stmts e            -> identifiersInScope pos stmts <> identifiersInScope pos e
            CS.IfThenElse _ e1 e2 e3     -> identifiersInScope pos e1 <> identifiersInScope pos e2 <> identifiersInScope pos e3
            CS.Case _ _ _ e as           -> identifiersInScope pos e <> identifiersInScope pos as
            _                            -> mempty
        | otherwise              = mempty

instance HasIdentifiersInScope a => HasIdentifiersInScope (CS.Field a) where
    identifiersInScope pos f@(CS.Field _ _ e)
        | f `containsPos` pos = identifiersInScope pos e
        | otherwise           = mempty

instance HasIdentifiersInScope (CS.Statement a) where
    identifiersInScope pos stmt
        | stmt `containsPos` pos = case stmt of
            CS.StmtExpr _ e    -> identifiersInScope pos e
            CS.StmtDecl _ _ ds -> identifiersInScope pos ds
            -- TODO: This exposes more variables than actually in scope, as bound
            --       variables are not accessible from within the expression (as
            --       opposed to e.g. let-bindings which allow recursion). However,
            --       shadowing should still work properly, e.g.
            --
            --           e <- let e = 3 in show e
            --                                  ^
            --       ...should cause the rightmost e to be referring to the let-bound
            --       e (rather than the statement-bound e).
            CS.StmtBind _ p e  -> insertAll (withName <$> identifiers p) $ identifiersInScope pos e
        | otherwise              = mempty

instance HasIdentifiersInScope (CS.Alt a) where
    identifiersInScope pos alt@(CS.Alt _ p rhs)
        | alt `containsPos` pos = insertAll (withName <$> identifiers p) $ identifiersInScope pos rhs
        | otherwise             = mempty

instance {-# OVERLAPPABLE #-} (Foldable f, Functor f, HasIdentifiersInScope a) => HasIdentifiersInScope (f a) where
    -- Later occurrences of variables in the Foldable are preferred
    -- due to (<>) being 'flipped'/right-biased in ConstMap compared
    -- to Data.Map's semigroup instance (which is left-biased).
    identifiersInScope pos = fold . (identifiersInScope pos <$>)
