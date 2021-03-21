{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Curry.LanguageServer.Handlers.Completion (completionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CT
import qualified Base.Types as CTY
import qualified CompilerEnv as CE
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Lens ((^.))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Conversions (ppToText)
import Curry.LanguageServer.Utils.Env (valueInfoType, typeInfoKind)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.List.Extra (nubOrd, nubOrdOn)
import Data.Maybe (maybeToList, fromMaybe)
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
        entry <- I.getModule normUri
        vfile <- MaybeT $ S.getVirtualFile normUri
        query <- MaybeT $ VFS.getCompletionPrefix pos vfile
        liftIO $ fetchCompletions entry query
    let maxCompletions = 25
        items = take maxCompletions completions
        incomplete = length completions > maxCompletions
        result = J.CompletionList incomplete $ J.List items
    responder $ Right $ J.InR result

fetchCompletions :: I.ModuleStoreEntry -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
fetchCompletions entry query
    | isPragma  = pragmaCompletions query
    | otherwise = generalCompletions entry query
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

generalCompletions :: I.ModuleStoreEntry -> VFS.PosPrefixInfo -> IO [J.CompletionItem]
generalCompletions entry query = do
    -- TODO: Context-awareness (through nested envs?)
    let env = maybeToList $ I.mseCompilerEnv entry
        valueCompletions   = toMatchingCompletions query $ nubOrdOn fst $ (CT.allBindings . CE.valueEnv)  =<< env
        typeCompletions    = toMatchingCompletions query $ nubOrdOn fst $ (CT.allBindings . CE.tyConsEnv) =<< env
        moduleCompletions  = toMatchingCompletions query $ nubOrd $ (maybeToList . CI.qidModule . fst) =<< (CT.allImports . CE.valueEnv) =<< env
        keywordCompletions = toMatchingCompletions query keywords
        completions        = valueCompletions ++ typeCompletions ++ moduleCompletions ++ keywordCompletions
    infoM "cls.completions" $ "Found " ++ show (length completions) ++ " completions with prefix '" ++ show (VFS.prefixText query) ++ "'"
    return completions
    where keywords = Keyword . T.pack <$> ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

toMatchingCompletions :: (ToCompletionItem a, CompletionQueryFilter a) => VFS.PosPrefixInfo -> [a] -> [J.CompletionItem]
toMatchingCompletions query = map (toCompletionItem query) . filter (matchesCompletionQuery query)

newtype Keyword = Keyword T.Text

class CompletionQueryFilter a where
    matchesCompletionQuery :: VFS.PosPrefixInfo -> a -> Bool

instance CompletionQueryFilter T.Text where
    matchesCompletionQuery query txt = VFS.prefixText query `T.isPrefixOf` txt && T.null (VFS.prefixModule query)

instance CompletionQueryFilter Keyword where
    matchesCompletionQuery query (Keyword txt) = matchesCompletionQuery query txt

instance CompletionQueryFilter (CI.QualIdent, a) where
    matchesCompletionQuery query (qid, _) = matchesCompletionQuery query qid

instance CompletionQueryFilter CI.QualIdent where
    matchesCompletionQuery query qid = (pfText `T.isPrefixOf` idText)
                                    && (T.null pfMod || pfMod == idMod)
        where pfText = VFS.prefixText query
              pfMod  = VFS.prefixModule query
              idText = T.pack $ CI.idName $ CI.qidIdent qid
              idMod  = T.pack $ maybe "" CI.moduleName $ CI.qidModule qid

instance CompletionQueryFilter CI.ModuleIdent where
    matchesCompletionQuery query mid = pfName `T.isPrefixOf` idName
        where idName = T.pack $ CI.moduleName mid
              pfText = VFS.prefixText query
              pfMod  = VFS.prefixModule query
              pfName | T.null pfMod = pfText
                     | otherwise    = pfMod <> "." <> pfText

-- TODO: Reimplement the following functions in terms of bindingToQualSymbols and a conversion from SymbolInformation to CompletionItem?

class ToCompletionItem a where
    toCompletionItem :: VFS.PosPrefixInfo -> a -> J.CompletionItem

instance ToCompletionItem (CI.QualIdent, CEV.ValueInfo) where
    -- | Converts a Curry value binding to a completion item.
    toCompletionItem query (qid, vinfo) = completionFrom name ciKind detail doc
        where fullName = ppToText qid
              name = fromMaybe fullName $ T.stripPrefix (VFS.prefixModule query <> ".") fullName
              ciKind = case vinfo of
                  CEV.DataConstructor _ _ _ _   -> J.CiEnumMember
                  CEV.NewtypeConstructor _ _ _  -> J.CiEnumMember
                  CEV.Value _ _ _ t | arity > 0 -> J.CiFunction
                                    | otherwise -> J.CiConstant
                      where arity = CTY.arrowArity $ CTY.rawType t
                  CEV.Label _ _ _              -> J.CiFunction -- Arity is always 1 for record labels
              vtype = valueInfoType vinfo
              detail = Just $ ppToText vtype
              doc = case vinfo of
                  CEV.DataConstructor _ _ recordLabels _ -> Just $ T.intercalate ", " $ ppToText <$> recordLabels
                  _                                      -> Nothing

instance ToCompletionItem (CI.QualIdent, CETC.TypeInfo) where
    -- | Converts a Curry type binding to a completion item.
    toCompletionItem query (qid, tinfo) = completionFrom name ciKind detail doc
        where fullName = ppToText qid
              name = fromMaybe fullName $ T.stripPrefix (VFS.prefixModule query <> ".") fullName
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

instance ToCompletionItem CI.ModuleIdent where
    -- | Creates a completion item from a module identifier.
    toCompletionItem query mid = completionFrom name ciKind detail doc
        where fullName = T.pack $ CI.moduleName mid
              name = fromMaybe fullName $ T.stripPrefix (VFS.prefixModule query <> ".") fullName
              ciKind = J.CiModule
              detail = Nothing
              doc = Nothing

instance ToCompletionItem Keyword where
    -- | Creates a completion item from a keyword.
    toCompletionItem _ (Keyword kw) = completionFrom kw ciKind detail doc
        where ciKind = J.CiKeyword
              detail = Nothing
              doc = Just "Keyword"

instance ToCompletionItem T.Text where
    toCompletionItem _ txt = completionFrom txt ciKind detail doc
        where ciKind = J.CiText
              detail = Nothing
              doc = Nothing

-- | Creates a completion item using the given label, kind, a detail and doc.
completionFrom :: T.Text -> J.CompletionItemKind -> Maybe T.Text -> Maybe T.Text -> J.CompletionItem
completionFrom l k d c = J.CompletionItem label kind tags detail doc deprecated
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
        additionalTextEdits = Nothing
        commitChars = Nothing
        command = Nothing
        xdata = Nothing

