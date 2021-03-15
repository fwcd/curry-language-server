module Curry.LanguageServer.Handlers.TextDocument (
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    didCloseHandler
) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Monad
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

didOpenHandler :: S.Handlers LSM
didOpenHandler = S.notificationHandler J.STextDocumentDidOpen $ \nt -> do
    liftIO $ debugM "cls.textDocument" "Processing open notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    void $ runMaybeT $ updateIndexStore uri

didChangeHandler :: S.Handlers LSM
didChangeHandler = S.notificationHandler J.STextDocumentDidChange $ \nt -> do
    liftIO $ debugM "cls.textDocument" "Processing change notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    void $ runMaybeT $ updateIndexStore uri

didSaveHandler :: S.Handlers LSM
didSaveHandler = S.notificationHandler J.STextDocumentDidSave $ \nt -> do
    liftIO $ debugM "cls.textDocument" "Processing save notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    void $ runMaybeT $ updateIndexStore uri

didCloseHandler :: S.Handlers LSM
didCloseHandler = S.notificationHandler J.STextDocumentDidClose $ \_nt -> do
    liftIO $ debugM "cls.textDocument" "Processing close notification"
    -- TODO: Remove file from LSM state?
    return ()

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> MaybeT LSM ()
updateIndexStore uri = do
    fl <- lift fileLoader
    cfg <- lift S.getConfig
    normUri <- liftIO $ normalizeUriWithPath uri
    I.recompileModule cfg fl normUri
    entry <- I.getModule normUri
    lift $ emitDiagnostics normUri entry

