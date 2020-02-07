module Curry.LanguageServer.Utils.Env (LookupValueInfo (..)) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified CompilerEnv as CE
import qualified Env.Value as CEV

import Data.Maybe (listToMaybe)

class LookupValueInfo i where
    lookupValueInfo :: CE.CompilerEnv -> i -> Maybe CEV.ValueInfo

instance LookupValueInfo CI.QualIdent where
    lookupValueInfo env ident = listToMaybe $ flip CEV.qualLookupValue (CE.valueEnv env) ident
