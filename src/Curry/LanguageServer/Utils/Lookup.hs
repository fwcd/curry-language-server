{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ViewPatterns #-}
-- | Position lookup in the AST.
module Curry.LanguageServer.Utils.Lookup
    ( findQualIdentAtPos
    , findTypeAtPos
    , findScopeAtPos
    , Scope
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State (State, when, execState, gets, modify)
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range)
import Curry.LanguageServer.Utils.General (rangeElem, joinFst, (<.$>))
import Curry.LanguageServer.Utils.Syntax
    ( elementAt
    , HasExpressions(..)
    , HasIdentifiers(..)
    , HasQualIdentifier(..)
    , HasQualIdentifiers(..)
    )
import Curry.LanguageServer.Utils.Sema
    ( HasTypedSpanInfos(typedSpanInfos), TypedSpanInfo )
import qualified Data.Map as M
import qualified Language.LSP.Types as J

-- | A collectScope of bound identifiers.
type Scope a = M.Map CI.Ident (Maybe a)

-- | Finds identifier and (occurrence) span info at a given position.
findQualIdentAtPos :: CS.Module a -> J.Position -> Maybe (CI.QualIdent, CSPI.SpanInfo)
findQualIdentAtPos ast pos = qualIdent <|> exprIdent <|> basicIdent
    where qualIdent = withSpanInfo <$> elementAt pos (qualIdentifiers ast)
          exprIdent = joinFst $ qualIdentifier <.$> withSpanInfo <$> elementAt pos (expressions ast)
          basicIdent = CI.qualify <.$> withSpanInfo <$> elementAt pos (identifiers ast)

-- | Finds the type at the given position.
findTypeAtPos :: CS.Module a -> J.Position -> Maybe (TypedSpanInfo a)
findTypeAtPos ast pos = elementAt pos $ typedSpanInfos ast

-- | Finds all accessible identifiers at the given position, using the innermost shadowed one.
findScopeAtPos :: CS.Module a -> J.Position -> Scope a
findScopeAtPos ast pos = sstMatchingEnv $ execState (collectScope ast) $ ScopeState
    { sstCurrentEnv = [M.empty]
    , sstMatchingEnv = M.empty
    , sstPosition = pos
    }

withSpanInfo :: CSPI.HasSpanInfo a => a -> (a, CSPI.SpanInfo)
withSpanInfo x = (x, CSPI.getSpanInfo x)

containsPos :: CSPI.HasSpanInfo a => a -> J.Position -> Bool
containsPos x pos = maybe False (rangeElem pos) $ currySpanInfo2Range x

-- | Binds an identifier in the innermost scope.
bindInScopes :: CI.Ident -> Maybe a -> [Scope a] -> [Scope a]
bindInScopes i t (sc:scs) = M.insert (CI.unRenameIdent i) t sc : scs
bindInScopes _ _ _        = error "Cannot bind without a scope!"

-- | Flattens the given scopes, preferring earlier binds.
flattenScopes :: [Scope a] -> Scope a
flattenScopes = foldr M.union M.empty

-- | Stores nested scopes and a cursor position. The head of the list is always the innermost collectScope.
data ScopeState a = ScopeState
    { sstCurrentEnv :: [Scope a]
    , sstMatchingEnv :: Scope a
    , sstPosition :: J.Position
    }

type ScopeM a = State (ScopeState a)

beginScope :: ScopeM a ()
beginScope = modify $ \s -> s { sstCurrentEnv = M.empty : sstCurrentEnv s }

endScope :: ScopeM a ()
endScope = modify $ \s -> s { sstCurrentEnv = let e = tail $ sstCurrentEnv s in if null e then error "Cannot end top-level scope!" else e }

withScope :: ScopeM a () -> ScopeM a ()
withScope x = beginScope >> x >> endScope

bind :: CI.Ident -> Maybe a -> ScopeM a ()
bind i t = do
    modify $ \s -> s { sstCurrentEnv = bindInScopes i t $ sstCurrentEnv s }

updateEnvs :: CSPI.HasSpanInfo e => e -> ScopeM a ()
updateEnvs (CSPI.getSpanInfo -> spi) = do
    pos <- gets sstPosition
    when (spi `containsPos` pos) $
        modify $ \s -> s { sstMatchingEnv = M.union (flattenScopes $ sstCurrentEnv s) $ sstMatchingEnv s }

class CollectScope e a where
    collectScope :: e -> ScopeM a ()

instance CollectScope (CS.Module a) a where
    collectScope (CS.Module _ _ _ _ _ _ decls) = collectScope $ TopDecl <$> decls

-- TopDecls introduce a new scope, LocalDecls don't
newtype TopDecl a = TopDecl (CS.Decl a)
newtype LocalDecl a = LocalDecl (CS.Decl a)

instance CollectScope (TopDecl a) a where
    collectScope (TopDecl decl) = (>> updateEnvs decl) $ withScope $ collectScope $ LocalDecl decl

instance CollectScope (LocalDecl a) a where
    collectScope (LocalDecl decl) = (>> updateEnvs decl) $ case decl of
        CS.FunctionDecl _ t i eqs    -> bind i (Just t) >> collectScope eqs
        CS.PatternDecl _ p rhs       -> collectScope p >> collectScope rhs
        CS.InstanceDecl _ _ _ _ _ ds -> collectScope $ TopDecl <$> ds
        CS.ClassDecl _ _ _ _ _ ds    -> collectScope $ TopDecl <$> ds
        _                            -> return ()

instance CollectScope (CS.Pattern a) a where
    collectScope pat = (>> updateEnvs pat) $ case pat of
        CS.VariablePattern _ t i        -> bind i $ Just t
        CS.ConstructorPattern _ _ _ ps  -> collectScope ps
        CS.InfixPattern _ _ p1 _ p2     -> collectScope p1 >> collectScope p2
        CS.ParenPattern _ p             -> collectScope p
        CS.RecordPattern _ _ _ fs       -> collectScope fs
        CS.TuplePattern _ ps            -> collectScope ps
        CS.ListPattern _ _ ps           -> collectScope ps
        CS.AsPattern _ i p              -> bind i Nothing >> collectScope p
        CS.LazyPattern _ p              -> collectScope p
        CS.FunctionPattern _ _ _ ps     -> collectScope ps
        CS.InfixFuncPattern _ _ p1 _ p2 -> collectScope p1 >> collectScope p2
        _                               -> return ()


instance CollectScope (CS.Equation a) a where
    collectScope eqn@(CS.Equation _ lhs rhs) = withScope $ collectScope lhs >> collectScope rhs >> updateEnvs eqn

instance CollectScope (CS.Lhs a) a where
    collectScope lhs = (>> updateEnvs lhs) $ case lhs of
        -- We don't need to collect the identifier since it's already bound in the FunctionDecl.
        CS.FunLhs _ _ ps   -> collectScope ps
        CS.OpLhs _ p1 _ p2 -> collectScope p1 >> collectScope p2
        CS.ApLhs _ l ps    -> collectScope l >> collectScope ps

instance CollectScope (CS.Rhs a) a where
    collectScope rhs = (>> updateEnvs rhs) $ case rhs of
        CS.SimpleRhs _ _ e ds   -> collectScope e >> collectScope (LocalDecl <$> ds)
        CS.GuardedRhs _ _ cs ds -> collectScope cs >> collectScope (LocalDecl <$> ds)

instance CollectScope (CS.CondExpr a) a where
    collectScope c@(CS.CondExpr _ e1 e2) = collectScope e1 >> collectScope e2 >> updateEnvs c

instance CollectScope (CS.Expression a) a where
    collectScope expr = (>> updateEnvs expr) $ case expr of
        CS.Paren _ e                 -> collectScope e
        CS.Typed _ e _               -> collectScope e
        CS.Record _ _ _ fs           -> collectScope fs
        CS.RecordUpdate _ e fs       -> collectScope e >> collectScope fs
        CS.Tuple _ es                -> collectScope es
        CS.List _ _ es               -> collectScope es
        CS.ListCompr _ e stmts       -> collectScope e >> collectScope stmts
        CS.EnumFrom _ e              -> collectScope e
        CS.EnumFromThen _ e1 e2      -> collectScope e1 >> collectScope e2
        CS.EnumFromTo _ e1 e2        -> collectScope e1 >> collectScope e2
        CS.EnumFromThenTo _ e1 e2 e3 -> collectScope e1 >> collectScope e2 >> collectScope e3
        CS.UnaryMinus _ e            -> collectScope e
        CS.Apply _ e1 e2             -> collectScope e1 >> collectScope e2
        CS.InfixApply _ e1 _ e2      -> collectScope e1 >> collectScope e2
        CS.LeftSection _ e _         -> collectScope e
        CS.RightSection _ _ e        -> collectScope e
        CS.Lambda _ ps e             -> withScope $ collectScope ps >> collectScope e
                                        -- We collect the scope twice to ensure that variables can
                                        -- be used before their declaration.
        CS.Let _ _ ds e              -> withScope $ collectScope ds' >> collectScope ds' >> collectScope e
            where ds' = LocalDecl <$> ds
        CS.Do _ _ stmts e            -> withScope $ collectScope stmts >> collectScope e
        CS.IfThenElse _ e1 e2 e3     -> collectScope e1 >> collectScope e2 >> collectScope e3
        CS.Case _ _ _ e as           -> collectScope e >> collectScope as
        _                            -> return ()

instance CollectScope e a => CollectScope (CS.Field e) a where
    collectScope f@(CS.Field _ _ e) = collectScope e >> updateEnvs f

instance CollectScope (CS.Statement a) a where
    collectScope stmt = (>> updateEnvs stmt) $ case stmt of
        CS.StmtExpr _ e    -> collectScope e
        CS.StmtDecl _ _ ds -> collectScope $ LocalDecl <$> ds
        CS.StmtBind _ p e  -> collectScope e >> collectScope p

instance CollectScope (CS.Alt a) a where
    collectScope alt@(CS.Alt _ p rhs) = withScope $ collectScope p >> collectScope rhs >> updateEnvs alt

instance {-# OVERLAPPABLE #-} (Foldable t, CollectScope e a) => CollectScope (t e) a where
    collectScope = mapM_ collectScope
