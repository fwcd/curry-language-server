{-# LANGUAGE OverloadedStrings #-}
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
import Curry.LanguageServer.Utils.General (rmDupsOn, liftMaybe)
import Curry.LanguageServer.Utils.Env (valueInfoType, typeInfoKind)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Curry.LanguageServer.Monad
import Data.Maybe (maybeToList)
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
        liftIO $ fetchCompletions entry (VFS.prefixText query) pos
    let maxCompletions = 25
        items = take maxCompletions completions
        incomplete = length completions > maxCompletions
        result = J.CompletionList incomplete $ J.List items
    responder $ Right $ J.InR result

fetchCompletions :: I.ModuleStoreEntry -> T.Text -> J.Position -> IO [J.CompletionItem]
fetchCompletions entry query _ = do
    -- TODO: Context-awareness (through nested envs?)
    let env = maybeToList $ I.mseCompilerEnv entry
        valueCompletions = valueBindingToCompletion <$> ((CT.allBindings . CE.valueEnv) =<< env)
        typeCompletions = typeBindingToCompletion <$> ((CT.allBindings . CE.tyConsEnv) =<< env)
        keywordCompletions = keywordToCompletion <$> keywords
        completions = rmDupsOn (^. J.label) $ filter (matchesQuery query) $ valueCompletions ++ typeCompletions ++ keywordCompletions
    infoM "cls.completions" $ "Found " ++ show (length completions) ++ " completions with query '" ++ show query ++ "'"
    return completions
    where keywords = ["case", "class", "data", "default", "deriving", "do", "else", "external", "fcase", "free", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "as", "ccall", "forall", "hiding", "interface", "primitive", "qualified"]

-- | Tests whether a completion item matches the user's query.
matchesQuery :: T.Text -> J.CompletionItem -> Bool
matchesQuery query item = query `T.isPrefixOf` (item ^. J.label)

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

-- TODO: Reimplement the following functions in terms of bindingToQualSymbols and a conversion from SymbolInformation to CompletionItem

-- | Converts a Curry value binding to a completion item.
valueBindingToCompletion :: (CI.QualIdent, CEV.ValueInfo) -> J.CompletionItem
valueBindingToCompletion (qident, vinfo) = item
    where name = T.pack $ CI.idName $ CI.qidIdent qident
          ciKind = case vinfo of
              CEV.DataConstructor _ _ _ _  -> J.CiEnumMember
              CEV.NewtypeConstructor _ _ _ -> J.CiEnumMember
              CEV.Value _ _ arity _        -> if arity > 0 then J.CiFunction
                                                           else J.CiConstant
              CEV.Label _ _ _              -> J.CiFunction -- Arity is always 1 for record labels
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
