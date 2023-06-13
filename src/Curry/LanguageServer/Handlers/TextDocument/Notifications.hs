{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.TextDocument.Notifications
    ( didOpenHandler
    , didChangeHandler
    , didSaveHandler
    , didCloseHandler
    ) where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (getDebouncers, modifyDebouncers, LSM)
import Curry.LanguageServer.Utils.Concurrent (debounceConst)
import Curry.LanguageServer.Utils.Logging (infoM, debugM)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J

didOpenHandler :: S.Handlers LSM
didOpenHandler = S.notificationHandler J.STextDocumentDidOpen $ \nt -> do
    debugM "Processing open notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didChangeHandler :: S.Handlers LSM
didChangeHandler = S.notificationHandler J.STextDocumentDidChange $ \nt -> do
    debugM "Processing change notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didSaveHandler :: S.Handlers LSM
didSaveHandler = S.notificationHandler J.STextDocumentDidSave $ \nt -> do
    debugM "Processing save notification"
    let uri = nt ^. J.params . J.textDocument . J.uri
    updateIndexStoreDebounced uri

didCloseHandler :: S.Handlers LSM
didCloseHandler = S.notificationHandler J.STextDocumentDidClose $ \nt -> do
    debugM "Processing close notification"
    -- TODO: Remove file from LSM state?
    let uri = nt ^. J.params . J.textDocument . J.uri
    removeDebouncer uri

-- | Recompiles and stores the updated compilation, (re)using a debounced version of the function.
updateIndexStoreDebounced :: J.Uri -> LSM ()
updateIndexStoreDebounced uri = do
    dbs <- getDebouncers

    when (M.notMember uri dbs) $ do
        -- TODO: Make this delay configurable, e.g. through a config option
        let delayMs = 500
        infoM $ "Creating debouncer for " <> J.getUri uri
        fresh <- debounceConst (delayMs * 1000) (void $ runMaybeT $ updateIndexStore uri)
        modifyDebouncers $ M.insert uri fresh

    (db, _) <- fromJust . M.lookup uri <$> getDebouncers
    liftIO db

-- | Removes the debouncer for the given URI.
removeDebouncer :: J.Uri -> LSM ()
removeDebouncer uri = do
    dbs <- getDebouncers
    infoM $ "Removing debouncer for " <> J.getUri uri

    -- Cancel old debouncer
    case M.lookup uri dbs of
        Just (_, canceller) -> liftIO canceller
        _                   -> return ()

    modifyDebouncers $ M.delete uri

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> MaybeT LSM ()
updateIndexStore uri = do
    fl <- lift fileLoader
    cfg <- lift S.getConfig
    normUri <- normalizeUriWithPath uri
    lift $ I.recompileModule cfg fl normUri
    entry <- I.getModule normUri
    lift $ emitDiagnostics normUri entry

