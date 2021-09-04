module Curry.LanguageServer.Handlers.SemanticTokens (semanticTokensHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Monad
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Utils.Convert (HasSemanticTokens (..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

semanticTokensHandler :: S.Handlers LSM
semanticTokensHandler = S.requestHandler J.STextDocumentSemanticTokensFull $ \req responder -> do
    liftIO $ debugM "cls.semanticTokens" "Processing semantic tokens request"
    let doc = req ^. J.params . J.textDocument
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    store <- getStore
    let tokens = fromMaybe [] $ fetchSemanticTokens =<< I.storedModule normUri store
    case J.makeSemanticTokens def tokens of
        Left e   -> responder $ Left $ J.ResponseError J.InternalError e Nothing
        Right ts -> responder $ Right $ Just ts

fetchSemanticTokens :: I.ModuleStoreEntry -> Maybe [J.SemanticTokenAbsolute]
fetchSemanticTokens entry = do
    ast <- I.mseModuleAST entry
    return $ semanticTokens ast
