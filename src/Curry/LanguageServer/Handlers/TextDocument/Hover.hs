{-# LANGUAGE FlexibleContexts, NumericUnderscores, OverloadedStrings, OverloadedRecordDot, TypeOperators, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Curry.LanguageServer.Handlers.TextDocument.Hover (hoverHandler) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Extension (ExtensionPoint (..), ExtensionOutputFormat (..), Extension (..))
import Curry.LanguageServer.Utils.Convert (ppPredTypeToText, currySpanInfo2Range, ppToText)
import Curry.LanguageServer.Index.Resolve (resolveAtPos)
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Curry.LanguageServer.Utils.Lookup (findTypeAtPos)
import Curry.LanguageServer.Index.Symbol (symbolParentIdent)
import Curry.LanguageServer.Utils.Syntax (moduleIdentifier)
import Curry.LanguageServer.Utils.Sema (ModuleAST, TypedSpanInfo (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath, uriToFilePath)
import Curry.LanguageServer.Monad (LSM, getStore)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (MonadLsp)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, proc)
import System.Timeout (timeout)

hoverHandler :: S.Handlers LSM
hoverHandler = S.requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
    debugM "Processing hover request"
    let pos = req ^. J.params . J.position
        uri = req ^. J.params . J.textDocument . J.uri
    normUri <- normalizeUriWithPath uri
    store <- getStore
    hover <- runMaybeT $ do
        entry <- I.getModule normUri
        MaybeT $ fetchHover store entry pos uri
    responder $ Right $ maybe (J.InR J.Null) J.InL hover

fetchHover :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> I.ModuleStoreEntry -> J.Position -> J.Uri -> m (Maybe J.Hover)
fetchHover store entry pos uri = runMaybeT $ do
    ast <- liftMaybe entry.moduleAST
    cfg <- lift S.getConfig
    let baseHover = maybeToList $ qualIdentHover store ast pos <|> typedSpanInfoHover ast pos
    extHovers <- mapMaybeM (extensionHover store ast pos uri) cfg.extensions
    hover <- liftMaybe . joinHovers $ baseHover ++ extHovers
    lift $ infoM $ "Found hover: " <> previewHover hover
    return hover

qualIdentHover :: I.IndexStore -> ModuleAST -> J.Position -> Maybe J.Hover
qualIdentHover store ast pos = do
    (symbols, range) <- resolveAtPos store ast pos
    s <- listToMaybe symbols

    let contents = J.InL $ J.mkMarkdownCodeBlock "curry" $ s.qualIdent <> maybe "" (" :: " <>) s.printedType

    return $ J.Hover contents $ Just range

typedSpanInfoHover :: ModuleAST -> J.Position -> Maybe J.Hover
typedSpanInfoHover ast@(moduleIdentifier -> mid) pos = do
    TypedSpanInfo txt t spi <- findTypeAtPos ast pos

    let contents = J.InL $ J.mkMarkdownCodeBlock "curry" $ txt <> " :: " <> maybe "?" (ppPredTypeToText mid) t
        range = currySpanInfo2Range spi

    return $ J.Hover contents range

extensionHover :: MonadIO m => I.IndexStore -> ModuleAST -> J.Position -> J.Uri -> Extension -> m (Maybe J.Hover)
extensionHover store ast@(moduleIdentifier -> mid) pos@(J.Position l c) uri e = case e.extensionPoint of
    ExtensionPointHover -> runMaybeT $ do
        let symbol         = listToMaybe . fst =<< resolveAtPos store ast pos
            timeoutSecs    = 10
            timeoutMicros  = timeoutSecs * 1_000_000
            templateParams = [ ("currentFile", T.pack (fromMaybe "" (uriToFilePath uri)))
                             , ("currentUri", J.getUri uri)
                             , ("currentModule", ppToText mid)
                             , ("line", T.pack (show l))
                             , ("column", T.pack (show c))
                             , ("type", fromMaybe "" ((.printedType) =<< symbol))
                             , ("identifier", maybe "" (.ident) symbol)
                             , ("module", maybe "" symbolParentIdent symbol)
                             , ("symbolKind", T.pack (show (maybe I.Other (.kind) symbol)))
                             ] :: [(T.Text, T.Text)]
            applyParam p   = T.replace ("{" <> p <> "}")
            evalTemplate t = foldr (uncurry applyParam) t templateParams
            procOpts       = proc (T.unpack e.executable) (T.unpack . (evalTemplate :: T.Text -> T.Text) <$> e.args)

        (exitCode, out, err) <- MaybeT $ liftIO $ timeout timeoutMicros $ readCreateProcessWithExitCode procOpts ""

        let simpleCodeBlock t
                | T.null t  = ""
                | otherwise =  "```\n" <> t <> "\n```"
        
        text <- case exitCode of
            ExitSuccess             -> return $ T.unlines
                [ "**" <> e.name <> "**"
                , ""
                , case e.outputFormat of
                    ExtensionOutputFormatMarkdown  -> T.pack out
                    _                              -> simpleCodeBlock (T.pack out)
                ]
            _ | e.showOutputOnError -> return $ T.unlines
                [ "_Extension **" <> e.name <> "** exited with " <> T.pack (show exitCode) <> "_"
                , ""
                , simpleCodeBlock (T.pack err)
                ]
              | otherwise           -> liftMaybe Nothing

        let contents = J.InL $ J.MarkupContent J.MarkupKind_Markdown text
        
        return $ J.Hover contents Nothing
    _                   -> return Nothing

previewHover :: J.Hover -> T.Text
previewHover = T.unlines . (previewMarkupContent <$>) . normalizeHoverContents . (^. J.contents)

previewMarkupContent :: J.MarkupContent -> T.Text
previewMarkupContent (J.MarkupContent k t) = case k of
    J.MarkupKind_Markdown  -> markdownToPlain t
    J.MarkupKind_PlainText -> t

joinHovers :: [J.Hover] -> Maybe J.Hover
joinHovers [] = Nothing
joinHovers hs = Just $ foldr1 mergeHovers hs

mergeHovers :: J.Hover -> J.Hover -> J.Hover
mergeHovers (J.Hover (normalizeHoverContents -> cs1) r1) (J.Hover (normalizeHoverContents -> cs2) r2) =
    J.Hover (J.InL (joinMarkupContent (cs1 ++ cs2))) (r1 <|> r2)

joinMarkupContent :: [J.MarkupContent] -> J.MarkupContent
joinMarkupContent [] = emptyMarkupContent
joinMarkupContent cs = foldr1 mergeMarkupContent cs

mergeMarkupContent :: J.MarkupContent -> J.MarkupContent -> J.MarkupContent
mergeMarkupContent (normalizeToMarkdown -> J.MarkupContent _ t1) (normalizeToMarkdown -> J.MarkupContent _ t2) =
    J.MarkupContent J.MarkupKind_Markdown $ T.unlines [t1, "", "---", "", t2]

emptyMarkupContent :: J.MarkupContent
emptyMarkupContent = J.MarkupContent J.MarkupKind_PlainText ""

normalizeToMarkdown :: J.MarkupContent -> J.MarkupContent
normalizeToMarkdown (J.MarkupContent k t) = case k of
    J.MarkupKind_Markdown  -> J.MarkupContent k t
    J.MarkupKind_PlainText -> J.mkMarkdownCodeBlock "text" t

normalizeHoverContents :: J.MarkupContent J.|? (J.MarkedString J.|? [J.MarkedString]) -> [J.MarkupContent]
normalizeHoverContents m = case m of
    J.InL c          -> [c]
    J.InR (J.InL s)  -> [markedStringToContent s]
    J.InR (J.InR ss) -> markedStringToContent <$> ss

markedStringToContent :: J.MarkedString -> J.MarkupContent
markedStringToContent (J.MarkedString m) = case m of
    J.InL t                                -> J.MarkupContent J.MarkupKind_PlainText t
    J.InR (J.MarkedStringWithLanguage l t) -> J.mkMarkdownCodeBlock l t

markdownToPlain :: T.Text -> T.Text
markdownToPlain t = T.intercalate ", " $ filter includeLine $ T.lines t
    where includeLine l = not ("```" `T.isPrefixOf` l || T.null l)
