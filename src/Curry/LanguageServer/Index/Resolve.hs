-- | Lookup and resolution with the index.
module Curry.LanguageServer.Index.Resolve
    ( resolveQualIdentAtPos
    , resolveInStore
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Syntax as CS

import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Lookup (findQualIdentAtPos)
import qualified Language.LSP.Types as J

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


