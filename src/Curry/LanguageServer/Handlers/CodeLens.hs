module Curry.LanguageServer.Handlers.CodeLens (codeLensHandler) where

import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Monad
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

codeLensHandler :: S.Handlers LSM
codeLensHandler = S.requestHandler J.STextDocumentCodeLens $ \req responder -> do
    liftIO $ debugM "cls.codeLens" "Processing code lens request"
    let lenses = []
    responder $ Right $ J.List lenses
