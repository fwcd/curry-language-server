{-# LANGUAGE NoFieldSelectors #-}
-- | Lookup and resolution with the index.
module Curry.LanguageServer.Index.Resolve
    ( resolveAtPos
    , resolveQualIdent
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Syntax as CS

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Index.Symbol (Symbol (..))
import Curry.LanguageServer.Utils.Convert (currySpanInfo2Range, currySpanInfo2Location, ppToText)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Lookup (findQualIdentAtPos, findExprIdentAtPos, findModuleIdentAtPos, findScopeAtPos)
import qualified Language.LSP.Protocol.Types as J
import Data.Default (Default(def))
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)

-- | Resolves the identifier at the given position.
resolveAtPos :: I.IndexStore -> ModuleAST -> J.Position -> Maybe ([I.Symbol], J.Range)
resolveAtPos store ast pos =  resolveLocalIdentAtPos        ast pos
                          <|> resolveQualIdentAtPos   store ast pos
                          <|> resolveModuleIdentAtPos store ast pos

-- | Resolves the qualified identifier at the given position.
resolveQualIdentAtPos :: I.IndexStore -> ModuleAST -> J.Position -> Maybe ([I.Symbol], J.Range)
resolveQualIdentAtPos store ast pos = do
    (qid, spi) <- findQualIdentAtPos ast pos
    range <- currySpanInfo2Range spi
    let symbols = resolveQualIdent store ast qid
    return (symbols, range)

-- | Resolves the module identifier at the given position.
resolveModuleIdentAtPos :: I.IndexStore -> ModuleAST -> J.Position -> Maybe ([I.Symbol], J.Range)
resolveModuleIdentAtPos store ast pos = do
    (mid, spi) <- findModuleIdentAtPos ast pos
    range <- currySpanInfo2Range spi
    let symbols = resolveModuleIdent store mid
    return (symbols, range)

-- | Resolves the local identifier at the given position.
resolveLocalIdentAtPos :: ModuleAST -> J.Position -> Maybe ([I.Symbol], J.Range)
resolveLocalIdentAtPos ast pos = do
    let scope = findScopeAtPos ast pos
    (qid, spi) <- findExprIdentAtPos ast pos
    range <- currySpanInfo2Range spi
    let symbols = [def { ident = ppToText lid
                       , qualIdent = ppToText lid
                       , printedType = ppToText <$> join lty
                       , location = unsafePerformIO (runMaybeT (currySpanInfo2Location lid)) -- SAFETY: We expect this conversion to be pure
                       }
                  | (lid, lty) <- M.toList scope
                  , CI.idName lid == CI.idName (CI.qidIdent qid)
                  ]
    return (symbols, range)

-- | Resolves the qualified identifier at the given position.
resolveQualIdent :: I.IndexStore -> ModuleAST -> CI.QualIdent -> [I.Symbol]
resolveQualIdent store (CS.Module _ _ _ mid _ imps _) qid = do
    -- TODO: Deal with aliases
    qid' <- qid : (flip CI.qualQualify qid <$> ([mid, CI.mkMIdent ["Prelude"]]
                                             ++ [mid' | CS.ImportDecl _ mid' _ _ _ <- imps]))
    tryFilterFromCurrySource $ I.storedSymbolsByQualIdent qid' store

-- | Resolves the module identifier at the given position.
resolveModuleIdent :: I.IndexStore -> CI.ModuleIdent -> [I.Symbol]
resolveModuleIdent store mid = tryFilterFromCurrySource $ I.storedModuleSymbolsByModuleIdent mid store

-- | Tries filtering symbols from a Curry source file.
tryFilterFromCurrySource :: [I.Symbol] -> [I.Symbol]
tryFilterFromCurrySource symbols | any I.symbolIsFromCurrySource symbols = filter I.symbolIsFromCurrySource symbols
                                 | otherwise                             = symbols
