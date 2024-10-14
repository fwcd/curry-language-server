{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Initialize (initializeHandler) where

import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.FileLoader (fileLoader)
import Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics)
import Curry.LanguageServer.Utils.Logging (infoM)
import qualified Curry.LanguageServer.Index.Store as I
import Curry.LanguageServer.Monad (LSM)
import Data.Maybe (maybeToList, fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Server as S

initializeHandler :: J.TMessage J.Method_Initialize -> LSM ()
initializeHandler req = do
    infoM "Building index store..."
    workspaceFolders <- fromMaybe [] <$> S.getWorkspaceFolders
    let folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath uri
        folders = maybeToList . folderToPath =<< workspaceFolders
    mapM_ addDirToIndexStore folders
    count <- I.getModuleCount
    infoM $ "Indexed " <> T.pack (show count) <> " files"

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: FilePath -> LSM ()
addDirToIndexStore dirPath = do
    fl <- fileLoader
    cfg <- S.getConfig
    I.addWorkspaceDir cfg fl dirPath
    entries <- I.getModuleList
    mapM_ (uncurry emitDiagnostics) entries
    