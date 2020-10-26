{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Completion (fetchCompletions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import Curry.Base.Monad (runCYIOIgnWarn)
import qualified Curry.Files.Filenames as CFN
import qualified Curry.Syntax as CS
import qualified Base.TopEnv as CT
import qualified Base.Types as CTY
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Applicative (Alternative (..))
import Control.Lens ((^.))
import Curry.LanguageServer.Compiler (parseCurryModule)
import Curry.LanguageServer.IndexStore (storedModules, IndexStore, ModuleStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions (ppToText)
import Curry.LanguageServer.Utils.General (rmDupsOn, wordAtPos, guarded)
import Curry.LanguageServer.Utils.Env (valueInfoType, typeInfoKind)
import Curry.LanguageServer.Utils.Syntax (elementAt, elementsAt, elementContains, HasExpressions (..), HasDeclarations (..))
import qualified Data.Map as M
import Data.Maybe (maybeToList, isJust)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

fetchCompletions :: IndexStore -> ModuleStoreEntry -> T.Text -> J.Position -> IO [J.CompletionItem]
fetchCompletions store entry content pos = do
    let query = maybe "" id $ wordAtPos pos content
        env = compilerEnv entry
        ast = moduleAST entry

    -- Workaround: Re-parse AST since stored AST is not updated after errors
    ast' <- case (\(CS.Module _ _ _ mid _ _ _) -> parseCurryModule CO.defaultOptions mid (T.unpack content) $ CFN.moduleNameToFile mid) <$> ast of
        Just p -> do
            out <- runCYIOIgnWarn p
            return $ case out of
                Right (_, m) -> Just m
                _ -> Nothing
        _ -> return Nothing
    
    let completions = maybe [] id
                   $  (takeIfNonEmpty $ expressionCompletions ast' pos)
                  <|> (takeIfNonEmpty $ importCompletions store ast' pos)
                  <|> (takeIfNonEmpty $ generalCompletions env ast' pos query)
    
    logs INFO $ "fetchCompletions: Found " ++ show (length completions) ++ " completions with query '" ++ show query
    return completions
    where takeIfNonEmpty = guarded (not . null)

expressionCompletions :: Maybe (CS.Module a) -> J.Position -> [J.CompletionItem]
expressionCompletions ast pos = do
    expr <- maybeToList $ elementAt pos =<< (expressions <$> ast)
    case expr of
        -- TODO: Implement expression-specific completions
        _ -> []

importCompletions :: IndexStore -> Maybe (CS.Module a) -> J.Position -> [J.CompletionItem]
importCompletions store ast pos = do
    CS.Module _ _ _ _ _ is _ <- maybeToList ast
    CS.ImportDecl _ mid _ _ _ <- maybeToList $ elementAt pos is
    let mName = T.pack $ CI.moduleName mid
    filter (\c -> mName `T.isPrefixOf` (c ^. J.label)) $ moduleCompletions store

moduleCompletions :: IndexStore -> [J.CompletionItem]
moduleCompletions store = moduleToCompletion <$> ((maybeToList . moduleAST) =<< snd <$> storedModules store)

generalCompletions :: Maybe CE.CompilerEnv -> Maybe (CS.Module a) -> J.Position -> T.Text -> [J.CompletionItem]
generalCompletions env ast pos query = rmDupsOn (^. J.label) $ filter (matchesQuery query)
                                                             $ valueCompletions env ++ localCompletions ast pos
                                                                                    ++ typeCompletions env
                                                                                    ++ keywordCompletions

-- TODO: Re-implement localCompletions using NestEnvs and bindings

localCompletions :: Maybe (CS.Module a) -> J.Position -> [J.CompletionItem]
localCompletions ast pos = declarationToCompletions =<< (elementsAt pos $ declarations =<< maybeToList ast)

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

-- | Converts a module to a completion item.
moduleToCompletion :: CS.Module a -> J.CompletionItem
moduleToCompletion (CS.Module _ _ _ mid _ _ _) = item
    where name = T.pack $ CI.moduleName mid
          ciKind = J.CiModule
          detail = Nothing
          doc = Nothing
          item = completionFrom name ciKind detail doc

-- | Converts a declaration to a completion item.
declarationToCompletions :: CS.Decl a -> [J.CompletionItem]
declarationToCompletions decl = (\n -> completionFrom n J.CiVariable Nothing Nothing) <$> names
    where names = T.pack <$> CI.idName <$> case decl of
            CS.FunctionDecl _ _ ident _ -> [ident]
            CS.ExternalDecl _ vars      -> (\(CS.Var _ ident) -> ident) <$> vars
            CS.FreeDecl _ vars          -> (\(CS.Var _ ident) -> ident) <$> vars
            _                           -> []

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
