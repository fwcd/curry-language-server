{-# LANGUAGE NoFieldSelectors, OverloadedStrings, OverloadedRecordDot, FlexibleContexts, FlexibleInstances, MultiWayIf #-}
module Curry.LanguageServer.Handlers.TextDocument.Completion (completionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Syntax as CS
import qualified Base.Types as CT

import Control.Lens ((^.), (?~))
import Control.Monad (join, guard)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.State.Class (get)
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Range)
import Curry.LanguageServer.Utils.General (filterF, lastSafe)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Syntax (HasIdentifiers (..))
import Curry.LanguageServer.Utils.Lookup (findScopeAtPos)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Utils.VFS (PosPrefixInfo (..), getCompletionPrefix)
import Curry.LanguageServer.Monad (LSM)
import Data.Bifunctor (first)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList, fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (MonadLsp)

completionHandler :: S.Handlers LSM
completionHandler = S.requestHandler J.SMethod_TextDocumentCompletion $ \req responder -> do
    debugM "Processing completion request"
    let uri = req ^. J.params . J.textDocument . J.uri
        pos = req ^. J.params . J.position
    normUri <- normalizeUriWithPath uri
    capabilities <- S.getClientCapabilities
    cfg <- S.getConfig
    completions <- fmap (join . maybeToList) $ runMaybeT $ do
        store <- get
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri

        let query = getCompletionPrefix pos vfile
            opts = CompletionOptions
                { useSnippets = cfg.useSnippetCompletions && fromMaybe False (do
                    docCapabilities <- capabilities ^. J.textDocument
                    cmCapabilities <- docCapabilities ^. J.completion
                    ciCapabilities <- cmCapabilities ^. J.completionItem
                    ciCapabilities ^. J.snippetSupport)
                }
        lift $ fetchCompletions opts entry store query
    let maxCompletions = 25
        items = take maxCompletions completions
        incomplete = length completions > maxCompletions
        result = J.CompletionList incomplete Nothing items
    responder $ Right $ J.InR $ J.InL result

fetchCompletions :: (MonadIO m, MonadLsp CFG.Config m) => CompletionOptions -> I.ModuleStoreEntry -> I.IndexStore -> PosPrefixInfo -> m [J.CompletionItem]
fetchCompletions opts entry store query
    | isPragma  = pragmaCompletions opts query
    | isImport  = importCompletions opts store query
    | otherwise = generalCompletions opts entry store query
    where line = query.fullLine
          isPragma = "{-#" `T.isPrefixOf` line
          isImport = "import " `T.isPrefixOf` line

pragmaCompletions :: MonadIO m => CompletionOptions -> PosPrefixInfo -> m [J.CompletionItem]
pragmaCompletions opts query
    | isLanguagePragma = return $ toMatchingCompletions opts query knownExtensions
    | isOptionPragma   = return []
    | otherwise        = return $ toMatchingCompletions opts query pragmaKeywords
    where line               = query.fullLine
          languagePragmaName = "LANGUAGE"
          optionPragmaPrefix = "OPTIONS_"
          languagePragma     = Tagged [] $ Keyword languagePragmaName
          knownTools         = [minBound..maxBound] :: [CS.KnownTool]
          optionPragmas      = makeToolOptionKeyword <$> knownTools
          makeToolOptionKeyword tool = Tagged tags $ Keyword $ optionPragmaPrefix <> T.pack (show tool)
            where tags = case tool of
                    CS.CYMAKE -> [J.CompletionItemTag_Deprecated]
                    _         -> []
          isLanguagePragma = languagePragmaName `T.isInfixOf` line
          isOptionPragma   = optionPragmaPrefix `T.isInfixOf` line
          pragmaKeywords   = languagePragma : optionPragmas
          knownExtensions  = Keyword . T.pack . show <$> ([minBound..maxBound] :: [CS.KnownExtension])

importCompletions :: (MonadIO m, MonadLsp CFG.Config m) => CompletionOptions -> I.IndexStore -> PosPrefixInfo -> m [J.CompletionItem]
importCompletions opts store query = do
    let modules            = nubOrdOn (.qualIdent) $ I.storedModuleSymbolsWithPrefix (fullPrefix query) store
        moduleCompletions  = toMatchingCompletions opts query $ (\s -> CompletionSymbol s Nothing Nothing) <$> modules
        keywordCompletions = toMatchingCompletions opts query $ Keyword <$> ["qualified", "as", "hiding"]
        completions        = moduleCompletions ++ keywordCompletions
    infoM $ "Found " <> T.pack (show (length completions)) <> " import completion(s)"
    return completions

generalCompletions :: (MonadIO m, MonadLsp CFG.Config m) => CompletionOptions -> I.ModuleStoreEntry -> I.IndexStore -> PosPrefixInfo -> m [J.CompletionItem]
generalCompletions opts entry store query = do
    let localIdentifiers   = join <$> maybe M.empty (`findScopeAtPos` query.cursorPos) entry.moduleAST
        localIdentifiers'  = M.fromList $ map (first ppToText) $ M.toList localIdentifiers
        localCompletions   = toMatchingCompletions opts query $ uncurry Local <$> M.toList localIdentifiers'
        symbols            = filter (flip M.notMember localIdentifiers' . (.ident)) $ nubOrdOn (.qualIdent)
                                                                                    $ I.storedSymbolsWithPrefix query.prefixText store
        symbolCompletions  = toMatchingCompletions opts query $ toCompletionSymbols entry =<< symbols
        keywordCompletions = toMatchingCompletions opts query keywords
        completions        = localCompletions ++ symbolCompletions ++ keywordCompletions
    infoM $ "Local identifiers in scope: " <> T.pack (show (M.keys localIdentifiers'))
    infoM $ "Found " <> T.pack (show (length completions)) <> " completion(s) with prefix '" <> T.pack (show query.prefixText) <> "'"
    return completions
    where keywords = Keyword <$> ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

toMatchingCompletions :: (ToCompletionItems a, CompletionQueryFilter a, Foldable t) => CompletionOptions -> PosPrefixInfo -> t a -> [J.CompletionItem]
toMatchingCompletions opts query = (toCompletionItems opts query =<<) . filterF (matchesCompletionQuery query)

newtype Keyword = Keyword T.Text

data Local = Local T.Text (Maybe CT.PredType)

data Tagged a = Tagged [J.CompletionItemTag] a

data CompletionSymbol = CompletionSymbol
    { -- The index symbol
      symbol :: I.Symbol
      -- The, possibly aliased, module name. Nothing means that the symbol is available unqualified.
    , moduleName :: Maybe T.Text
      -- Import edits to apply after the completion has been selected. Nothing means that the symbol does not require an import.
    , importEdits :: Maybe [J.TextEdit]
    }

newtype CompletionOptions = CompletionOptions
    { useSnippets :: Bool
    }

-- | Turns an index symbol into completion symbols by analyzing the module's imports.
toCompletionSymbols :: I.ModuleStoreEntry -> I.Symbol -> [CompletionSymbol]
toCompletionSymbols entry s = do
    CS.Module _ _ _ mid _ imps _ <- maybeToList entry.moduleAST
    let pre = "Prelude"
        impNames = S.fromList [ppToText mid' | CS.ImportDecl _ mid' _ _ _ <- imps]

    if | s.kind == I.Module -> return CompletionSymbol
            { symbol = s
            , moduleName = Nothing
            , importEdits = Nothing
            }
       | (I.symbolParentIdent s == pre && pre `S.notMember` impNames) || I.symbolParentIdent s == ppToText mid -> do
            m <- [Nothing, Just $ I.symbolParentIdent s]
            return CompletionSymbol
                { symbol = s
                , moduleName = m
                , importEdits = Nothing
                }
       | otherwise -> do
            CS.ImportDecl _ mid' isQual alias spec <- imps
            guard $ ppToText mid' == I.symbolParentIdent s

            let isImported = case spec of
                    Just (CS.Importing _ is) -> flip S.member $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Just (CS.Hiding _ is)    -> flip S.notMember $ S.fromList $ ppToText <$> (identifiers =<< is)
                    Nothing                  -> const True
                moduleNames = (Just $ ppToText $ fromMaybe mid' alias) : [Nothing | not isQual]

            m <- moduleNames
            return CompletionSymbol
                { symbol = s
                , moduleName = m
                , importEdits = if isImported s.ident
                    then Nothing
                    else case spec of
                        Just (CS.Importing _ is) -> do
                            J.Range _ pos <- currySpanInfo2Range =<< lastSafe is
                            let range = J.Range pos pos
                                text | null is   = s.ident
                                     | otherwise = ", " <> s.ident
                                edit = J.TextEdit range text
                            return [edit]
                        _                        -> return []
                }


-- | The fully qualified, possibly aliased, name of the completion symbol.
fullName :: CompletionSymbol -> T.Text
fullName cms | s.kind == I.Module = s.qualIdent
             | otherwise          = maybe "" (<> ".") moduleName <> s.ident
    where s = cms.symbol
          moduleName = cms.moduleName

-- | The fully qualified prefix of the completion query.
fullPrefix :: PosPrefixInfo -> T.Text
fullPrefix query | T.null query.prefixScope = query.prefixText
                 | otherwise                = query.prefixScope <> "." <> query.prefixText

class CompletionQueryFilter a where
    matchesCompletionQuery :: PosPrefixInfo -> a -> Bool

instance CompletionQueryFilter T.Text where
    matchesCompletionQuery query txt = query.prefixText `T.isPrefixOf` txt && T.null query.prefixScope

instance CompletionQueryFilter Keyword where
    matchesCompletionQuery query (Keyword txt) = matchesCompletionQuery query txt

instance CompletionQueryFilter Local where
    matchesCompletionQuery query (Local i _) = query.prefixText `T.isPrefixOf` i

instance CompletionQueryFilter a => CompletionQueryFilter (Tagged a) where
    matchesCompletionQuery query (Tagged _ x) = matchesCompletionQuery query x

instance CompletionQueryFilter CompletionSymbol where
    matchesCompletionQuery query cms = fullPrefix query `T.isPrefixOf` fullName cms

class ToCompletionItems a where
    toCompletionItems :: CompletionOptions -> PosPrefixInfo -> a -> [J.CompletionItem]

instance ToCompletionItems CompletionSymbol where
    -- | Converts a Curry value binding to a completion item.
    toCompletionItems opts query cms = [makeCompletion name ciKind detail doc insertText insertTextFormat edits]
        where s = cms.symbol
              edits = cms.importEdits
              name = fromMaybe (fullName cms) $ T.stripPrefix (query.prefixScope <> ".") $ fullName cms
              ciKind = case s.kind of
                  I.ValueFunction    | s.arrowArity == Just 0 -> J.CompletionItemKind_Constant
                                     | otherwise              -> J.CompletionItemKind_Function
                  I.ValueConstructor | s.arrowArity == Just 0 -> J.CompletionItemKind_EnumMember
                                     | otherwise              -> J.CompletionItemKind_Constructor
                  I.Module                                    -> J.CompletionItemKind_Module
                  I.TypeData | length s.constructors == 1     -> J.CompletionItemKind_Struct
                             | otherwise                      -> J.CompletionItemKind_Enum
                  I.TypeNew                                   -> J.CompletionItemKind_Struct
                  I.TypeAlias                                 -> J.CompletionItemKind_Interface
                  I.TypeClass                                 -> J.CompletionItemKind_Interface
                  I.TypeVar                                   -> J.CompletionItemKind_Variable
                  I.Other                                     -> J.CompletionItemKind_Text
              insertText | opts.useSnippets = Just $ makeSnippet name s.printedArgumentTypes
                         | otherwise        = Just name
              insertTextFormat | opts.useSnippets = Just J.InsertTextFormat_Snippet
                               | otherwise        = Just J.InsertTextFormat_PlainText
              detail = s.printedType
              doc = Just $ T.intercalate "\n\n" $ filter (not . T.null)
                  [ if isNothing edits then "" else "_requires import_"
                  , T.intercalate ", " s.constructors
                  ]

instance ToCompletionItems Keyword where
    -- | Creates a completion item from a keyword.
    toCompletionItems _ _ (Keyword kw) = [makeCompletion label ciKind detail doc insertText insertTextFormat edits]
        where label = kw
              ciKind = J.CompletionItemKind_Keyword
              detail = Nothing
              doc = Just "Keyword"
              insertText = Just kw
              insertTextFormat = Just J.InsertTextFormat_PlainText
              edits = Nothing

instance ToCompletionItems Local where
    -- | Creates a completion item from a local variable.
    toCompletionItems opts _ (Local i t) = [makeCompletion label ciKind detail doc insertText insertTextFormat edits]
        where label = i
              ciKind = J.CompletionItemKind_Variable
              detail = ppToText <$> t
              doc = Just "Local"
              argTypes = (ppToText <$>) $ CT.arrowArgs . CT.unpredType =<< maybeToList t
              insertText | opts.useSnippets = Just $ makeSnippet i argTypes
                         | otherwise        = Just i
              insertTextFormat | opts.useSnippets = Just J.InsertTextFormat_Snippet
                               | otherwise        = Just J.InsertTextFormat_PlainText
              edits = Nothing

instance ToCompletionItems T.Text where
    toCompletionItems _ _ txt = [makeCompletion label ciKind detail doc insertText insertTextFormat edits]
        where label = txt
              ciKind = J.CompletionItemKind_Text
              detail = Nothing
              doc = Nothing
              insertText = Just txt
              insertTextFormat = Just J.InsertTextFormat_PlainText
              edits = Nothing

instance ToCompletionItems a => ToCompletionItems (Tagged a) where
    toCompletionItems opts query (Tagged tags x) = (J.tags ?~ tags) <$> toCompletionItems opts query x

-- | Creates a snippet with VSCode-style syntax.
makeSnippet :: T.Text -> [T.Text] -> T.Text
makeSnippet name ts = T.intercalate " " $ name : ((\(i, t) -> "${" <> T.pack (show (i :: Int)) <> ":" <> t <> "}") <$> zip [1..] ts)

-- | Creates a completion item using the given label, kind, a detail and doc.
makeCompletion :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe J.InsertTextFormat -> Maybe [J.TextEdit] -> J.CompletionItem
makeCompletion l k d c it itf es = J.CompletionItem label labelDetails kind tags detail doc deprecated
                                          preselect sortText filterText insertText
                                          insertTextFormat insertTextMode textEdit textEditText
                                          additionalTextEdits commitChars command xdata
  where label = l
        labelDetails = Nothing
        kind = Just k
        tags = Nothing
        detail = d
        doc = J.InR . J.MarkupContent J.MarkupKind_Markdown <$> c
        deprecated = Just False
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = it
        insertTextFormat = itf
        insertTextMode = Nothing
        textEdit = Nothing
        textEditText = Nothing
        additionalTextEdits = es
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

