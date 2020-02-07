module Curry.LanguageServer.Utils.Env (
    LookupValueInfo (..),
    LookupEnv,
    LM,
    runLM,
    findAtPos
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified CompilerEnv as CE
import qualified Env.Value as CEV

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.Compiler (ModuleAST)
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Data.Maybe (listToMaybe)
import qualified Language.Haskell.LSP.Types as J

type LookupEnv = (CE.CompilerEnv, ModuleAST)
type LM a = MaybeT (Reader LookupEnv) a

-- | Runs the lookup monad with the given environment and
-- module identifier.
runLM :: LM a -> CE.CompilerEnv -> ModuleAST -> Maybe a
runLM lm = curry $ runReader $ runMaybeT lm

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

class LookupValueInfo i where
    lookupValueInfo :: i -> LM CEV.ValueInfo

instance LookupValueInfo CI.QualIdent where
    lookupValueInfo ident = do
        (env, _) <- ask
        liftMaybe $ listToMaybe $ flip CEV.qualLookupValue (CE.valueEnv env) ident
