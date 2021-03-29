-- | (Value) environments and position lookup in the AST.
module Curry.LanguageServer.Index.Lookup (
    findQualIdentAtPos,
    findTypeAtPos
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.Types as CT

import Control.Applicative
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
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
