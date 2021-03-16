module Curry.LanguageServer.Handlers.CodeLens (codeLensHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Conversions (HasCodeLenses(..))
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

codeLensHandler :: S.Handlers LSM
codeLensHandler = S.requestHandler J.STextDocumentCodeLens $ \req responder -> do
    liftIO $ debugM "cls.codeLens" "Processing code lens request"
    let J.CodeLensParams _ _ doc = req ^. J.params
        uri = doc ^. J.uri
    normUri <- liftIO $ normalizeUriWithPath uri
    lenses <- runMaybeT $ do
        entry <- I.getModule normUri
        liftIO $ fetchCodeLenses entry
    responder $ Right $ J.List $ fromMaybe [] lenses

fetchCodeLenses :: I.ModuleStoreEntry -> IO [J.CodeLens]
fetchCodeLenses entry = do
    let lenses = maybe [] codeLenses $ I.mseModuleAST entry
    debugM "cls.codeLens" $ "Found " ++ show (length lenses) ++ " code lenses"
    return lenses
