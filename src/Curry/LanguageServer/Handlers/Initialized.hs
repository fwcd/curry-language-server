module Curry.LanguageServer.Handlers.Initialized (initializedHandler) where

import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.LogHandler (setupLogging)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad
import Data.Maybe (maybeToList, fromMaybe)
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.Log.Logger

initializedHandler :: S.Handlers LSM
initializedHandler = S.notificationHandler J.SInitialized $ \_nt -> do
    setupLogging
    liftIO $ infoM "cls.initialized" "Building index store..."
    workspaceFolders <- fromMaybe [] <$> S.getWorkspaceFolders
    let folders = maybeToList . folderToPath =<< workspaceFolders
    mapM_ addDirToIndexStore folders
    count <- I.getModuleCount
    liftIO $ infoM "cls.initialized" $ "Indexed " ++ show count ++ " files"
    where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: FilePath -> LSM ()
addDirToIndexStore dirPath = do
    fl <- fileLoader
    cfg <- S.getConfig
    I.addWorkspaceDir cfg fl dirPath
    entries <- I.getModuleList
    mapM_ (uncurry emitDiagnostics) entries
    