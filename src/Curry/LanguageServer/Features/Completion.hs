{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Completion (fetchCompletions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CT
import qualified Base.Types as CTY
import qualified CompilerEnv as CE
import qualified Env.Value as CEV

import Control.Lens
import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions (ppToText)
import Curry.LanguageServer.Utils.General (rmDupsOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

fetchCompletions :: IndexStoreEntry -> T.Text -> J.Position -> IO [J.CompletionItem]
fetchCompletions entry query pos = do
    -- TODO: Context-awareness (through nested envs?)
    let env = maybeToList $ compilerEnv entry
        smartCompletions = rmDupsOn (^. J.label) $ bindingToCompletion <$> ((CT.allBindings . CE.valueEnv) =<< env)
        keywordCompletions = keywordToCompletion <$> keywords
        completions = filter (matchesQuery query) $ smartCompletions ++ keywordCompletions
    logs INFO $ "fetchCompletions: Found " ++ show (length smartCompletions) ++ " smart completions with query '" ++ show query ++ "'"
    return completions
    where keywords = ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

-- | Tests whether a completion item matches the user's query.
matchesQuery :: T.Text -> J.CompletionItem -> Bool
matchesQuery query item = query `T.isPrefixOf` (item ^. J.label)

-- | Creates a completion item using the given label, kind, a detail and doc.
completionFrom :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> J.CompletionItem
completionFrom label ciKind detail doc = J.CompletionItem label (Just ciKind) detail (J.CompletionDocString <$> doc) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Converts a Curry binding to a completion item.
bindingToCompletion :: (CI.QualIdent, CEV.ValueInfo) -> J.CompletionItem
bindingToCompletion (qident, vinfo) = item
    where name = T.pack $ CI.idName $ CI.qidIdent qident
          ciKind = case vinfo of
              CEV.DataConstructor _ arity _ _ -> if arity > 1 then J.CiEnum
                                                              else J.CiStruct
              CEV.NewtypeConstructor _ _ _    -> J.CiStruct
              -- TODO: Workaround, since constrainted arities are not properly included in ValueInfo
              CEV.Value _ _ _ t               -> if CTY.arrowArity ty > 0 then J.CiFunction
                                                                          else J.CiConstant
                                                    -- TODO: CTY.TypeScheme/CTY.PredType have been replaced by CTY.Type in newer versions of curry-frontend
                                                    where (CTY.ForAll _ (CTY.PredType _ ty)) = t
                                                                
              CEV.Label _ _ _                 -> J.CiFunction -- Arity is always 1 for record labels
          vtype = case vinfo of
              CEV.DataConstructor _ _ _ t  -> t
              CEV.NewtypeConstructor _ _ t -> t
              CEV.Value _ _ _ t            -> t
              CEV.Label _ _ t              -> t
          detail = Just $ ppToText vtype
          doc = case vinfo of
              CEV.DataConstructor _ _ recordLabels _ -> Just $ T.intercalate ", " $ T.pack <$> CI.idName <$> recordLabels
              _                                      -> Nothing
          item = completionFrom name ciKind detail doc

-- | Creates a completion item from a keyword.
keywordToCompletion :: T.Text -> J.CompletionItem
keywordToCompletion kw = completionFrom kw J.CiKeyword Nothing $ Just "Keyword"
