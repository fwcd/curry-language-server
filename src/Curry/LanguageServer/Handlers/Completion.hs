{-# LANGUAGE OverloadedStrings, FlexibleInstances, ViewPatterns, MultiWayIf #-}
module Curry.LanguageServer.Handlers.Completion (completionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Control.Lens ((^.))
import Control.Monad (join, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.State.Class (get)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Range)
import Curry.LanguageServer.Utils.General (ConstMap (..))
import Curry.LanguageServer.Utils.Syntax (HasIdentifiers (..))
import Curry.LanguageServer.Utils.Lookup (HasIdentifiersInScope (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList, fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.VFS as VFS
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

completionHandler :: S.Handlers LSM
completionHandler = S.requestHandler J.STextDocumentCompletion $ \req responder -> do
    liftIO $ debugM "cls.completions" "Processing completion request"
    let uri = req ^. J.params . J.textDocument . J.uri
        pos = req ^. J.params . J.position
    normUri <- liftIO $ normalizeUriWithPath uri
    capabilities <- S.getClientCapabilities
    completions <- fmap (join . maybeToList) $ runMaybeT $ do
        store <- get
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri
        query <- MaybeT $ VFS.getCompletionPrefix pos vfile
        
        let opts = CompletionOptions
                { cmoUseSnippets = fromMaybe False $ do
                    docCapabilities <- capabilities ^. J.textDocument
                    cmCapabilities <- docCapabilities ^. J.completion
                    ciCapabilities <- cmCapabilities ^. J.completionItem
                    ciCapabilities ^. J.snippetSupport
                }
        liftIO $ fetchCompletions opts entry store query
    let maxCompletions = 25
        items = take maxCompletions completions
        incomplete = length completions > maxCompletions
        result = J.CompletionList incomplete $ J.List items
    responder $ Right $ J.InR result

fetchCompletions :: CompletionOptions -> I.ModuleStoreEntry -> I.IndexStore -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
fetchCompletions opts entry store query
    | isPragma  = pragmaCompletions opts query
    | isImport  = importCompletions opts store query
    | otherwise = generalCompletions opts entry store query
    where line = VFS.fullLine query
          isPragma = "{-#" `T.isPrefixOf` line
          isImport = "import " `T.isPrefixOf` line

pragmaCompletions :: CompletionOptions -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
pragmaCompletions opts query
    | isLanguagePragma = return $ toMatchingCompletions opts query $ Keyword <$> knownExtensions
    | isOptionPragma   = return []
    | otherwise        = return $ toMatchingCompletions opts query $ Keyword <$> pragmaKinds
    where line = VFS.fullLine query
          languagePragma = "LANGUAGE"
          optionPragmas = ("OPTIONS_" <>) <$> ["KICS2", "PAKCS", "CYMAKE", "FRONTEND" :: T.Text]
          isLanguagePragma = languagePragma `T.isInfixOf` line
          isOptionPragma = any (`T.isInfixOf` line) optionPragmas
          pragmaKinds = languagePragma : optionPragmas
          knownExtensions = ["AnonFreeVars", "CPP", "FunctionalPatterns", "NegativeLiterals", "NoImplicitPrelude" :: T.Text]

importCompletions :: CompletionOptions -> I.IndexStore -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
importCompletions opts store query = do
    let modules            = nubOrdOn I.sQualIdent $ I.storedModuleSymbolsWithPrefix (fullPrefix query) store
        moduleCompletions  = toMatchingCompletions opts query $ (\s -> CompletionSymbol s Nothing Nothing) <$> modules
        keywordCompletions = toMatchingCompletions opts query $ Keyword <$> ["qualified", "as", "hiding"]
        completions        = moduleCompletions ++ keywordCompletions
    infoM "cls.completions" $ "Found " ++ show (length completions) ++ " import completion(s)"
    return completions

generalCompletions :: CompletionOptions -> I.ModuleStoreEntry -> I.IndexStore -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
generalCompletions opts entry store query = do
    let ast                = I.mseModuleAST entry
        localIdentifiers   = identifiersInScope (VFS.cursorPos query) ast
        localCompletions   = [] -- TODO: Context-awareness (through nested envs?)
        symbolCompletions  = toMatchingCompletions opts query $ toCompletionSymbols entry =<< nubOrdOn I.sQualIdent (I.storedSymbolsWithPrefix (VFS.prefixText query) store)
        keywordCompletions = toMatchingCompletions opts query keywords
        completions        = localCompletions ++ symbolCompletions ++ keywordCompletions
    infoM "cls.completions" $ "Local identifiers in scope: " ++ show (M.keys $ ctmMap localIdentifiers)
    infoM "cls.completions" $ "Found " ++ show (length completions) ++ " completion(s) with prefix '" ++ show (VFS.prefixText query) ++ "'"
    return completions
    where keywords = Keyword <$> ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

toMatchingCompletions :: (ToCompletionItems a, CompletionQueryFilter a) => CompletionOptions -> VFS.PosPrefixInfo -> [a] -> [J.CompletionItem]
toMatchingCompletions opts query = (toCompletionItems opts query =<<) . filter (matchesCompletionQuery query)

newtype Keyword = Keyword T.Text

data CompletionSymbol = CompletionSymbol
    { -- The index symbol
      cmsSymbol :: I.Symbol
      -- The, possibly aliased, module name. Nothing means that the symbol is available unqualified.
    , cmsModuleName :: Maybe T.Text
      -- Import edits to apply after the completion has been selected. Nothing means that the symbol does not require an import.
    , cmsImportEdits :: Maybe [J.TextEdit]
    }

newtype CompletionOptions = CompletionOptions
    { cmoUseSnippets :: Bool
    }

-- | Turns an index symbol into completion symbols by analyzing the module's imports.
toCompletionSymbols :: I.ModuleStoreEntry -> I.Symbol -> [CompletionSymbol]
toCompletionSymbols entry s = do
    CS.Module _ _ _ mid _ imps _ <- maybeToList $ I.mseModuleAST entry
    let pre = "Prelude"
        impNames = S.fromList [ppToText mid' | CS.ImportDecl _ mid' _ _ _ <- imps]
    
    if | I.sKind s == I.Module -> return CompletionSymbol
            { cmsSymbol = s
            , cmsModuleName = Nothing
            , cmsImportEdits = Nothing
            }
       | (I.sParentIdent s == pre && pre `S.notMember` impNames) || I.sParentIdent s == ppToText mid -> do
            m <- [Nothing, Just $ I.sParentIdent s]
            return CompletionSymbol
                { cmsSymbol = s
                , cmsModuleName = m
                , cmsImportEdits = Nothing
                }
       | otherwise -> do
            CS.ImportDecl _ mid' isQual alias spec <- imps
            guard $ ppToText mid' == I.sParentIdent s

            let isImported = case spec of
                    Just (CS.Importing _ is) -> flip S.member $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Just (CS.Hiding _ is)    -> flip S.notMember $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Nothing                  -> const True
                moduleNames = (Just $ ppToText $ fromMaybe mid' alias) : [Nothing | not isQual]
            
            m <- moduleNames
            return CompletionSymbol
                { cmsSymbol = s
                , cmsModuleName = m
                , cmsImportEdits = if isImported $ I.sIdent s
                    then Nothing
                    else Just $ case spec of
                        Just (CS.Importing (currySpanInfo2Range -> Just (J.Range _ pos)) is) -> let range = J.Range pos pos
                                                                                                    text | null is   = I.sIdent s
                                                                                                         | otherwise = ", " <> I.sIdent s
                                                                                                    edit = J.TextEdit range text
                                                                                                in [edit]
                        _                                                                    -> []
                }


-- | The fully qualified, possibly aliased, name of the completion symbol.
fullName :: CompletionSymbol -> T.Text
fullName cms | I.sKind s == I.Module = I.sQualIdent s
             | otherwise             = maybe "" (<> ".") moduleName <> I.sIdent s
    where s = cmsSymbol cms
          moduleName = cmsModuleName cms

-- | The fully qualified prefix of the completion query.
fullPrefix :: VFS.PosPrefixInfo -> T.Text
fullPrefix query | T.null (VFS.prefixModule query) = VFS.prefixText query
                 | otherwise                       = VFS.prefixModule query <> "." <> VFS.prefixText query

class CompletionQueryFilter a where
    matchesCompletionQuery :: VFS.PosPrefixInfo -> a -> Bool

instance CompletionQueryFilter T.Text where
    matchesCompletionQuery query txt = VFS.prefixText query `T.isPrefixOf` txt && T.null (VFS.prefixModule query)

instance CompletionQueryFilter Keyword where
    matchesCompletionQuery query (Keyword txt) = matchesCompletionQuery query txt

instance CompletionQueryFilter CompletionSymbol where
    matchesCompletionQuery query cms = fullPrefix query `T.isPrefixOf` fullName cms

class ToCompletionItems a where
    toCompletionItems :: CompletionOptions -> VFS.PosPrefixInfo -> a -> [J.CompletionItem]

instance ToCompletionItems CompletionSymbol where
    -- | Converts a Curry value binding to a completion item.
    toCompletionItems opts query cms = [completionFrom name ciKind detail doc insertText insertTextFormat edits]
        where s = cmsSymbol cms
              edits = cmsImportEdits cms
              name = fromMaybe (fullName cms) $ T.stripPrefix (VFS.prefixModule query <> ".") $ fullName cms
              ciKind = case I.sKind s of
                  I.ValueFunction    | I.sArrowArity s == Just 0 -> J.CiConstant
                                     | otherwise                 -> J.CiFunction
                  I.ValueConstructor | I.sArrowArity s == Just 0 -> J.CiEnumMember
                                     | otherwise                 -> J.CiConstructor
                  I.Module                                       -> J.CiModule
                  I.TypeData | length (I.sConstructors s) == 1   -> J.CiStruct
                             | otherwise                         -> J.CiEnum
                  I.TypeNew                                      -> J.CiStruct
                  I.TypeAlias                                    -> J.CiInterface
                  I.TypeClass                                    -> J.CiInterface
                  I.TypeVar                                      -> J.CiVariable
                  I.Other                                        -> J.CiText
              insertText | cmoUseSnippets opts = Just $ T.intercalate " " $ name : ((\(i, t) -> "${" <> T.pack (show (i :: Int)) <> ":" <> t <> "}") <$> zip [1..] (I.sPrintedArgumentTypes s))
                         | otherwise           = Just name
              insertTextFormat | cmoUseSnippets opts = Just J.Snippet
                               | otherwise           = Just J.PlainText
              detail = I.sPrintedType s
              doc = Just $ T.intercalate "\n\n" $ filter (not . T.null)
                  [ if isNothing edits then "" else "_requires import_"
                  , T.intercalate ", " $ I.sConstructors s
                  ]

instance ToCompletionItems Keyword where
    -- | Creates a completion item from a keyword.
    toCompletionItems _ _ (Keyword kw) = [completionFrom label ciKind detail doc insertText insertTextFormat edits]
        where label = kw
              ciKind = J.CiKeyword
              detail = Nothing
              doc = Just "Keyword"
              insertText = Just kw
              insertTextFormat = Just J.PlainText
              edits = Nothing

instance ToCompletionItems T.Text where
    toCompletionItems _ _ txt = [completionFrom label ciKind detail doc insertText insertTextFormat edits]
        where label = txt
              ciKind = J.CiText
              detail = Nothing
              doc = Nothing
              insertText = Just txt
              insertTextFormat = Just J.PlainText
              edits = Nothing

-- | Creates a completion item using the given label, kind, a detail and doc.
completionFrom :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe J.InsertTextFormat -> Maybe [J.TextEdit] -> J.CompletionItem
completionFrom l k d c it itf es = J.CompletionItem label kind tags detail doc deprecated
                                          preselect sortText filterText insertText
                                          insertTextFormat textEdit additionalTextEdits
                                          commitChars command xdata
  where label = l
        kind = Just k
        tags = Nothing
        detail = d
        doc = J.CompletionDocMarkup . J.MarkupContent J.MkMarkdown <$> c
        deprecated = Just False
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = it
        insertTextFormat = itf
        textEdit = Nothing
        additionalTextEdits = J.List <$> es
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

