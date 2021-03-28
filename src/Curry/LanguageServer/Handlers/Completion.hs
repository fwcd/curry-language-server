{-# LANGUAGE OverloadedStrings, FlexibleInstances, ViewPatterns #-}
module Curry.LanguageServer.Handlers.Completion (completionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS

import Control.Lens ((^.))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.State.Class (get)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Range)
import Curry.LanguageServer.Utils.Syntax (HasIdentifiers (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.Maybe (maybeToList, fromMaybe, isNothing)
import Data.List.Extra (nubOrdOn)
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
    completions <- fmap (join . maybeToList) $ runMaybeT $ do
        store <- get
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri
        query <- MaybeT $ VFS.getCompletionPrefix pos vfile
        liftIO $ fetchCompletions entry store query
    let maxCompletions = 25
        items = take maxCompletions completions
        incomplete = length completions > maxCompletions
        result = J.CompletionList incomplete $ J.List items
    responder $ Right $ J.InR result

fetchCompletions :: I.ModuleStoreEntry -> I.IndexStore -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
fetchCompletions entry store query
    | isPragma  = pragmaCompletions query
    | otherwise = generalCompletions entry store query
    where line = VFS.fullLine query
          isPragma = "{-#" `T.isPrefixOf` line

pragmaCompletions :: VFS.PosPrefixInfo -> IO [J.CompletionItem]
pragmaCompletions query
    | isLanguagePragma = return $ toMatchingCompletions query $ Keyword <$> knownExtensions
    | isOptionPragma   = return []
    | otherwise        = return $ toMatchingCompletions query $ Keyword <$> pragmaKinds
    where line = VFS.fullLine query
          languagePragma = "LANGUAGE"
          optionPragmas = ("OPTIONS_" <>) <$> ["KICS2", "PAKCS", "CYMAKE", "FRONTEND" :: T.Text]
          isLanguagePragma = languagePragma `T.isInfixOf` line
          isOptionPragma = any (`T.isInfixOf` line) optionPragmas
          pragmaKinds = languagePragma : optionPragmas
          knownExtensions = ["AnonFreeVars", "CPP", "FunctionalPatterns", "NegativeLiterals", "NoImplicitPrelude" :: T.Text]

generalCompletions :: I.ModuleStoreEntry -> I.IndexStore -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
generalCompletions entry store query = do
    let localCompletions   = [] -- TODO: Context-awareness (through nested envs?)
        symbolCompletions  = toMatchingCompletions query $ toCompletionSymbols entry =<< I.storedSymbolsWithPrefix (VFS.prefixText query) store -- TODO: Direct qualified symbol completions?
        keywordCompletions = toMatchingCompletions query keywords
        completions        = localCompletions ++ symbolCompletions ++ keywordCompletions
    infoM "cls.completions" $ "Found " ++ show (length completions) ++ " completions with prefix '" ++ show (VFS.prefixText query) ++ "'"
    return completions
    where keywords = Keyword . T.pack <$> ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

toMatchingCompletions :: (ToCompletionItems a, CompletionQueryFilter a) => VFS.PosPrefixInfo -> [a] -> [J.CompletionItem]
toMatchingCompletions query = (toCompletionItems query =<<) . filter (matchesCompletionQuery query)

newtype Keyword = Keyword T.Text

data CompletionSymbol = CompletionSymbol
    { cmsSymbol :: I.Symbol
    , cmsModuleName :: Maybe T.Text -- possibly aliased
    , cmsImportEdits :: Maybe [J.TextEdit]
    }

toCompletionSymbols :: I.ModuleStoreEntry -> I.Symbol -> [CompletionSymbol]
toCompletionSymbols entry s = nubOrdOn (I.sQualIdent . cmsSymbol) $ do
    CS.Module _ _ _ mid _ imps _ <- maybeToList $ I.mseModuleAST entry
    
    if I.sParentIdent s == "Prelude" || I.sParentIdent s == ppToText mid
        then [ CompletionSymbol
                { cmsSymbol = s
                , cmsModuleName = m
                , cmsImportEdits = Nothing
                } | m <- [Nothing, Just $ I.sParentIdent s]]
        else do
            CS.ImportDecl _ mid' isQual alias spec <- imps
            let isImported = case spec of
                    Just (CS.Importing _ is) -> flip S.member $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Just (CS.Hiding _ is)    -> flip S.notMember $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Nothing                  -> const True
                moduleNames = (Just $ ppToText $ fromMaybe mid' alias) : [Nothing | not isQual]
            [CompletionSymbol
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
                } | m <- moduleNames]

class CompletionQueryFilter a where
    matchesCompletionQuery :: VFS.PosPrefixInfo -> a -> Bool

instance CompletionQueryFilter T.Text where
    matchesCompletionQuery query txt = VFS.prefixText query `T.isPrefixOf` txt && T.null (VFS.prefixModule query)

instance CompletionQueryFilter Keyword where
    matchesCompletionQuery query (Keyword txt) = matchesCompletionQuery query txt

instance CompletionQueryFilter CompletionSymbol where
    matchesCompletionQuery query cms = fullPrefix `T.isPrefixOf` fullName
        where s = cmsSymbol cms
              moduleName = cmsModuleName cms
              fullName = maybe "" (<> ".") moduleName <> I.sIdent s
              fullPrefix | T.null (VFS.prefixModule query) = VFS.prefixText query
                         | otherwise                       = VFS.prefixModule query <> "." <> VFS.prefixText query

class ToCompletionItems a where
    toCompletionItems :: VFS.PosPrefixInfo -> a -> [J.CompletionItem]

instance ToCompletionItems CompletionSymbol where
    -- | Converts a Curry value binding to a completion item.
    toCompletionItems query cms = [completionFrom name ciKind detail doc edits]
        where s = cmsSymbol cms
              moduleName = cmsModuleName cms
              edits = cmsImportEdits cms
              fullName = maybe "" (<> ".") moduleName <> I.sIdent s
              name = fromMaybe fullName $ T.stripPrefix (VFS.prefixModule query <> ".") fullName
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
              detail = I.sPrintedType s
              doc = Just $ T.intercalate "\n" $ filter (not . T.null)
                  [ if isNothing edits then "" else "_requires import_"
                  , T.intercalate ", " $ I.sConstructors s
                  ]

instance ToCompletionItems Keyword where
    -- | Creates a completion item from a keyword.
    toCompletionItems _ (Keyword kw) = [completionFrom kw ciKind detail doc Nothing]
        where ciKind = J.CiKeyword
              detail = Nothing
              doc = Just "Keyword"

instance ToCompletionItems T.Text where
    toCompletionItems _ txt = [completionFrom txt ciKind detail doc Nothing]
        where ciKind = J.CiText
              detail = Nothing
              doc = Nothing

-- | Creates a completion item using the given label, kind, a detail and doc.
completionFrom :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> Maybe [J.TextEdit] -> J.CompletionItem
completionFrom l k d c es = J.CompletionItem label kind tags detail doc deprecated
                                          preselect sortText filterText insertText
                                          insertTextFormat textEdit additionalTextEdits
                                          commitChars command xdata
  where label = l
        kind = Just k
        tags = Nothing
        detail = d
        doc = J.CompletionDocString <$> c
        deprecated = Just False
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = Nothing
        insertTextFormat = Nothing
        textEdit = Nothing
        additionalTextEdits = J.List <$> es
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

