{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Completion (fetchCompletions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CT
import qualified Base.Types as CTY
import qualified CompilerEnv as CE
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Applicative (Alternative (..))
import Control.Lens ((^.))
import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions (ppToText)
import Curry.LanguageServer.Utils.General (rmDupsOn)
import Curry.LanguageServer.Utils.Env (valueInfoType, typeInfoKind)
import Curry.LanguageServer.Utils.Syntax (elementAt, ModuleAST, HasExpressions (..), HasDeclarations (..))
import qualified Data.Map as M
import Data.Maybe (maybeToList, isJust)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

fetchCompletions :: ModuleStoreEntry -> T.Text -> J.Position -> IO [J.CompletionItem]
fetchCompletions entry query pos = do
    let env = compilerEnv entry
        ast = moduleAST entry
        completions = expressionCompletions ast pos
                  <|> declarationCompletions ast pos
                  <|> generalCompletions env query
    
    logs INFO $ "fetchCompletions: Found " ++ show (length completions) ++ " completions with query '" ++ show query
    return completions

expressionCompletions :: Maybe ModuleAST -> J.Position -> [J.CompletionItem]
expressionCompletions ast pos = do
    expr <- maybeToList $ elementAt pos =<< (expressions <$> ast)
    case expr of
        -- TODO: Implement expression completions
        _ -> []

declarationCompletions :: Maybe ModuleAST -> J.Position -> [J.CompletionItem]
declarationCompletions ast pos = do
    expr <- maybeToList $ elementAt pos =<< (declarations <$> ast)
    case expr of
        -- TODO: Implement declaration completions
        _ -> []

generalCompletions :: Maybe CE.CompilerEnv -> T.Text -> [J.CompletionItem]
generalCompletions env query = rmDupsOn (^. J.label) $ filter (matchesQuery query) $ valueCompletions env ++ typeCompletions env ++ keywordCompletions

valueCompletions :: Maybe CE.CompilerEnv -> [J.CompletionItem]
valueCompletions env = valueBindingToCompletion <$> ((CT.allBindings . CE.valueEnv) =<< maybeToList env)

typeCompletions :: Maybe CE.CompilerEnv -> [J.CompletionItem]
typeCompletions env = typeBindingToCompletion <$> ((CT.allBindings . CE.tyConsEnv) =<< maybeToList env)

keywordCompletions :: [J.CompletionItem]
keywordCompletions = keywordToCompletion <$> keywords
    where keywords = ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

-- | Tests whether a completion item matches the user's query.
matchesQuery :: T.Text -> J.CompletionItem -> Bool
matchesQuery query item = query `T.isPrefixOf` (item ^. J.label)

-- | Creates a completion item using the given label, kind, a detail and doc.
completionFrom :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> J.CompletionItem
completionFrom label ciKind detail doc = J.CompletionItem label (Just ciKind) detail (J.CompletionDocString <$> doc) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- TODO: Reimplement the following functions in terms of bindingToQualSymbols and a conversion from SymbolInformation to CompletionItem

-- | Converts a Curry value binding to a completion item.
valueBindingToCompletion :: (CI.QualIdent, CEV.ValueInfo) -> J.CompletionItem
valueBindingToCompletion (qident, vinfo) = item
    where name = T.pack $ CI.idName $ CI.qidIdent qident
          ciKind = case vinfo of
              CEV.DataConstructor _ arity _ _ -> J.CiEnumMember
              CEV.NewtypeConstructor _ _ _    -> J.CiEnumMember
              CEV.Value _ _ arity _           -> if arity > 0 then J.CiFunction
                                                              else J.CiConstant
              CEV.Label _ _ _                 -> J.CiFunction -- Arity is always 1 for record labels
          vtype = valueInfoType vinfo
          detail = Just $ ppToText vtype
          doc = case vinfo of
              CEV.DataConstructor _ _ recordLabels _ -> Just $ T.intercalate ", " $ ppToText <$> recordLabels
              _                                      -> Nothing
          item = completionFrom name ciKind detail doc

-- | Converts a Curry type binding to a completion item.
typeBindingToCompletion :: (CI.QualIdent, CETC.TypeInfo) -> J.CompletionItem
typeBindingToCompletion (qident, tinfo) = item
    where name = T.pack $ CI.idName $ CI.qidIdent qident
          ciKind = case tinfo of
              CETC.DataType _ _ _     -> J.CiStruct
              CETC.RenamingType _ _ _ -> J.CiInterface
              CETC.AliasType _ _ _ _  -> J.CiInterface
              CETC.TypeClass _ _ _    -> J.CiInterface
              CETC.TypeVar _          -> J.CiTypeParameter
          tkind = typeInfoKind tinfo
          detail = Just $ ppToText tkind
          doc = case tinfo of
              CETC.DataType _ _ cs    -> Just $ T.intercalate ", " $ ppToText <$> CTY.constrIdent <$> cs
              CETC.RenamingType _ _ c -> Just $ ppToText c
              CETC.AliasType _ _ _ t  -> Just $ ppToText t
              _                       -> Nothing
          item = completionFrom name ciKind detail doc

-- | Creates a completion item from a keyword.
keywordToCompletion :: T.Text -> J.CompletionItem
keywordToCompletion kw = completionFrom kw J.CiKeyword Nothing $ Just "Keyword"
