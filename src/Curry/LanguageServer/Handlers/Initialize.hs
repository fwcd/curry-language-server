{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Initialize (initializedHandler) where

import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import Curry.LanguageServer.Utils.Logging (infoM)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (LSM)
import Data.Maybe (maybeToList, fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J

initializedHandler :: S.Handlers LSM
initializedHandler = S.notificationHandler J.SMethod_Initialized $ \_nt -> do
    infoM "Building index store..."
    workspaceFolders <- fromMaybe [] <$> S.getWorkspaceFolders
    let folders = maybeToList . folderToPath =<< workspaceFolders
    mapM_ addDirToIndexStore folders
    count <- I.getModuleCount
    infoM $ "Indexed " <> T.pack (show count) <> " files"
    where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath uri

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: FilePath -> LSM ()
addDirToIndexStore dirPath = do
    fl <- fileLoader
    cfg <- S.getConfig
    I.addWorkspaceDir cfg fl dirPath
    entries <- I.getModuleList
    mapM_ (uncurry emitDiagnostics) entries
    