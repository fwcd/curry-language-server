{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Curry.LanguageServer.Handlers.Definition (definitionHandler) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CT

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (filterM)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Env
import Curry.LanguageServer.Utils.General (liftMaybe)
import Curry.LanguageServer.Utils.Syntax (moduleIdentifier)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath, filePathToUri)
import Curry.LanguageServer.Monad
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import Data.List (find, intercalate)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.FilePath ((</>), pathSeparator, (<.>))
import System.Directory (doesFileExist)
import System.Log.Logger

definitionHandler :: S.Handlers LSM
definitionHandler = S.requestHandler J.STextDocumentDefinition $ \req responder -> do
    liftIO $ debugM "cls.definition" "Processing definition request"
    -- TODO: Update once https://github.com/haskell/lsp/issues/303 is fixed
    let J.DefinitionParams doc pos _ _ = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    defs <- runMaybeT $ do
        liftIO $ debugM "cls.definition" $ "Looking up " ++ show normUri ++ " in " ++ show (M.keys $ I.idxModules store)
        entry <- I.getModule normUri
        liftIO $ fetchDefinitions entry pos
    responder $ Right $ J.InR $ J.InR $ J.List $ fromMaybe [] defs

fetchDefinitions :: I.ModuleStoreEntry -> J.Position -> IO [J.LocationLink]
fetchDefinitions entry pos = do
    defs <- runMaybeT $ do ast <- liftMaybe $ I.mseModuleAST entry
                           env <- liftMaybe $ I.mseCompilerEnv entry
                           MaybeT $ runLM (definition entry pos) env ast
    infoM "cls.definition" $ "Found " ++ show defs
    return $ maybeToList defs

-- | Finds a definition at the given position.
definition :: I.ModuleStoreEntry -> J.Position -> LM J.LocationLink
definition entry pos = do
    (qident, _) <- liftMaybe =<< findQualIdentAtPos pos
    valueQIdent <- (CT.origName <$>) <$> lookupValueInfo qident
    typeQIdent  <- (CT.origName <$>) <$> lookupTypeInfo qident
    let origIdent = fromMaybe qident (valueQIdent <|> typeQIdent)
    origIdent'  <- resolveSourceFile entry origIdent
    liftMaybe =<< (liftIO $ runMaybeT $ currySpanInfos2LocationLink qident origIdent')

-- | Tries to attach a .curry source file to the given identifier.
resolveSourceFile :: I.ModuleStoreEntry -> CI.QualIdent -> LM CI.Ident
resolveSourceFile entry qid@(CI.qidIdent -> ident) = lift $ (fromMaybe ident <$>) $ runMaybeT $ do
    uri <- liftMaybe =<< (liftIO $ runMaybeT $ currySpanInfo2Uri ident)
    if ".curry" `T.isSuffixOf` J.getUri uri
        then return ident
        else do
           mid  <- liftMaybe $ CI.qidModule qid
           uri' <- locateSourceFile entry mid
           -- FIXME: While the new URI might be correct, the definition line/col will not match up currently
           liftMaybe $ setCurrySpanInfoUri uri' ident

-- | Tries to find a .curry source file for the given module identifier in the store.
locateSourceFile :: I.ModuleStoreEntry -> CI.ModuleIdent -> LM J.Uri
locateSourceFile entry mid = do
    let quals = CI.midQualifiers mid
        candidates = (<.> "curry") <$> (</> intercalate [pathSeparator] quals) <$> I.mseImportPaths entry
    liftIO $ infoM "cls.definition" $ "Candidates: " ++ show candidates
    fp <- liftMaybe =<< (liftIO $ listToMaybe <$> filterM doesFileExist candidates)
    uri <- liftIO $ filePathToUri fp
    liftIO $ infoM "cls.definition" $ "Found: " ++ show uri
    return uri
