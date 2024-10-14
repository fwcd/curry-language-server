{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Initialized (initializedHandler) where

import Control.Monad (void, forM_)
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
import System.FilePath (takeFileName)

initializedHandler :: S.Handlers LSM
initializedHandler = S.notificationHandler J.SMethod_Initialized $ \_nt -> do
    infoM "Building index store..."
    workspaceFolders <- fromMaybe [] <$> S.getWorkspaceFolders
    let folders = maybeToList . folderToPath =<< workspaceFolders
        folderCount = length folders

    void $ S.withProgress "Curry: Adding folders" Nothing S.NotCancellable $ \update ->
        forM_ (zip [0..] folders) $ \(i, fp) -> do
            let percent = (i * 100) `div` folderCount
                msg = T.pack $ "[" ++ show (i + 1) ++ " of " ++ show folderCount ++ "] " ++ takeFileName fp
            update (S.ProgressAmount (Just $ fromIntegral percent) $ Just msg)
            addDirToIndexStore fp

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
    