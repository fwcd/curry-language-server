-- | (Value) environments and position lookup in the AST.
module Curry.LanguageServer.Index.Lookup (
    findQualIdentAtPos,
    resolveQualIdentAtPos,
    findTypeAtPos,
    resolveInStore
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Applicative
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range)
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Curry.LanguageServer.Utils.Sema
import qualified Language.LSP.Types as J

-- | Finds identifier and (occurrence) span info at a given position.
findQualIdentAtPos :: ModuleAST -> J.Position -> Maybe (CI.QualIdent, CSPI.SpanInfo)
findQualIdentAtPos ast pos = qualIdent <|> exprIdent <|> basicIdent
    where qualIdent = withSpanInfo <$> elementAt pos (qualIdentifiers ast)
          exprIdent = joinFst $ qualIdentifier <.$> withSpanInfo <$> elementAt pos (expressions ast)
          basicIdent = CI.qualify <.$> withSpanInfo <$> elementAt pos (identifiers ast)

-- | Resolves the qualified identifier at the given position.
resolveQualIdentAtPos :: I.IndexStore -> ModuleAST -> J.Position -> Maybe ([I.Symbol], J.Range)
resolveQualIdentAtPos store ast@(CS.Module _ _ _ mid _ imps _) pos = do
    (qid, spi) <- findQualIdentAtPos ast pos
    range <- currySpanInfo2Range spi
    let symbols = do -- TODO: Deal with aliases
                     qid' <- qid : (flip CI.qualQualify qid <$> ([mid, CI.mkMIdent ["Prelude"]]
                                                              ++ [mid' | CS.ImportDecl _ mid' _ _ _ <- imps]))
                     resolveInStore store qid'
    return (symbols, range)

resolveInStore :: I.IndexStore -> CI.QualIdent -> [I.Symbol]
resolveInStore store qid = symbols'
    where symbols = I.storedSymbolsByQualIdent qid store
          symbols' | any I.sIsFromCurrySource symbols = filter I.sIsFromCurrySource symbols
                   | otherwise                        = symbols


-- | Finds the type at the given position.
findTypeAtPos :: ModuleAST -> J.Position -> Maybe (TypedSpanInfo CT.PredType)
findTypeAtPos ast pos = elementAt pos $ typedSpanInfos ast

withSpanInfo :: CSPI.HasSpanInfo a => a -> (a, CSPI.SpanInfo)
withSpanInfo x = (x, CSPI.getSpanInfo x)
