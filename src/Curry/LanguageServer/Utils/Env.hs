-- | (Value) environments and position lookup in the AST.
module Curry.LanguageServer.Utils.Env (
    CanLookupValueInfo (..),
    CanLookupTypeInfo (..),
    LookupEnv,
    LM,
    runLM,
    findQualIdentAtPos,
    findTypeAtPos,
    valueInfoType,
    typeInfoKind
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.Kinds as CK
import qualified Base.Types as CT
import qualified CompilerEnv as CE
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Data.Maybe (listToMaybe)
import qualified Language.LSP.Types as J

type LookupEnv = (CE.CompilerEnv, ModuleAST)
type LM a = MaybeT (ReaderT LookupEnv IO) a

-- | Runs the lookup monad with the given environment and
-- module identifier.
runLM :: LM a -> CE.CompilerEnv -> ModuleAST -> IO (Maybe a)
runLM lm = curry $ runReaderT $ runMaybeT lm

-- | Finds identifier and (occurrence) span info at a given position.
findQualIdentAtPos :: J.Position -> LM (Maybe (CI.QualIdent, CSPI.SpanInfo))
findQualIdentAtPos pos = do
    (_, ast) <- lift ask
    let mid = moduleIdentifier ast
        qualIdent = withSpanInfo <$> elementAt pos (qualIdentifiers ast)
        exprIdent = joinFst $ qualIdentifier <.$> withSpanInfo <$> elementAt pos (expressions ast)
    return $ CI.qualQualify mid <.$> (qualIdent <|> exprIdent)

-- | Finds the type at the given position.
findTypeAtPos :: J.Position -> LM (Maybe (TypedSpanInfo CT.PredType))
findTypeAtPos pos = do
    (_, ast) <- lift ask
    let typedSpanInfo = elementAt pos $ typedSpanInfos ast
    return typedSpanInfo

withSpanInfo :: CSPI.HasSpanInfo a => a -> (a, CSPI.SpanInfo)
withSpanInfo x = (x, CSPI.getSpanInfo x)

-- | Fetches the type from a value info.
valueInfoType :: CEV.ValueInfo -> CT.TypeScheme
valueInfoType vinfo = case vinfo of
    CEV.DataConstructor _ _ _ t  -> t
    CEV.NewtypeConstructor _ _ t -> t
    CEV.Value _ _ _ t            -> t
    CEV.Label _ _ t              -> t

-- | Fetches the kind from a type info.
typeInfoKind :: CETC.TypeInfo -> CK.Kind
typeInfoKind tinfo = case tinfo of
    CETC.DataType _ k _     -> k
    CETC.RenamingType _ k _ -> k
    CETC.AliasType _ k _ _  -> k
    CETC.TypeClass _ k _    -> k
    CETC.TypeVar k          -> k

class CanLookupValueInfo i where
    lookupValueInfo :: i -> LM (Maybe CEV.ValueInfo)

instance CanLookupValueInfo CI.QualIdent where
    lookupValueInfo ident = do
        (env, ast) <- ask
        let mident = moduleIdentifier ast
        return $ listToMaybe $ CEV.qualLookupValueUnique mident ident $ CE.valueEnv env

class CanLookupTypeInfo i where
    lookupTypeInfo :: i -> LM (Maybe CETC.TypeInfo)

instance CanLookupTypeInfo CI.QualIdent where
    lookupTypeInfo ident = do
        (env, ast) <- ask
        let mident = moduleIdentifier ast
        return $ listToMaybe $ CETC.qualLookupTypeInfoUnique mident ident $ CE.tyConsEnv env

