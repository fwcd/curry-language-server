-- | Utilities for extracting semantic information from the AST.
module Curry.LanguageServer.Utils.Sema (
    untypedTopLevelDecls
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.Types as CT
import qualified Curry.Syntax as CS

import qualified Data.Set as S

-- | Finds top-level function declarations in the module without an explicit type signature.
untypedTopLevelDecls :: CS.Module a -> [(CSPI.SpanInfo, CI.Ident, a)]
untypedTopLevelDecls (CS.Module _ _ _ _ _ _ decls) = untypedDecls
    where typeSigIdents = S.fromList [i | CS.TypeSig _ is _ <- decls, i <- is]
          untypedDecls = [(spi, i, t) | CS.FunctionDecl spi t i _ <- decls, i `S.notMember` typeSigIdents]
