-- | (Value) environments and position lookup in the AST.
module Curry.LanguageServer.Utils.Env (
    CanLookupValueInfo (..),
    LookupEnv,
    LM,
    runLM,
    findAtPos,
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
import qualified Language.Haskell.LSP.Types as J

type LookupEnv = (CE.CompilerEnv, ModuleAST)
type LM a = MaybeT (ReaderT LookupEnv IO) a

-- | Runs the lookup monad with the given environment and
-- module identifier.
runLM :: LM a -> CE.CompilerEnv -> ModuleAST -> IO (Maybe a)
runLM lm = curry $ runReaderT $ runMaybeT lm

-- | Finds identifier and (occurrence) span info at a given position.
findAtPos :: J.Position -> LM (CI.QualIdent, CSPI.SpanInfo)
findAtPos pos = do
    (env, ast) <- lift ask
    let mident = moduleIdentifier ast
        exprIdent = joinFst $ qualIdentifier <.$> (withSpanInfo <$> (elementAt pos $ expressions ast))
        declIdent = CI.qualifyWith mident <.$> (joinFst $ identifier <.$> (withSpanInfo <$> (elementAt pos $ declarations ast)))
    liftMaybe $ exprIdent <|> declIdent

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
    lookupValueInfo :: i -> LM CEV.ValueInfo

instance CanLookupValueInfo CI.QualIdent where
    lookupValueInfo ident = do
        (env, ast) <- ask
        let mident = moduleIdentifier ast
        liftMaybe $ listToMaybe $ CEV.qualLookupValueUnique mident ident $ CE.valueEnv env
