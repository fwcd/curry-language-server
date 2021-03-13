{-# LANGUAGE TupleSections #-}
module Curry.LanguageServer.Reactor (reactor, ReactorInput (..)) where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Handlers.Completion
import Curry.LanguageServer.Handlers.Definition
import Curry.LanguageServer.Handlers.Diagnostics
import Curry.LanguageServer.Handlers.DocumentSymbols
import Curry.LanguageServer.Handlers.Hover
import Curry.LanguageServer.Handlers.WorkspaceSymbols
import Curry.LanguageServer.Utils.General (liftMaybe, slipr3, wordAtPos)
import Curry.LanguageServer.Utils.Uri (filePathToNormalizedUri, normalizeUriWithPath)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.SortedList as SL
import Language.LSP.Diagnostics
import qualified Language.LSP.VFS as VFS
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The input to the reactor.
type ReactorInput = FromClientMessage

-- | The reactor monad holding a readable environment of LSP functions (with config)
-- and a state containing the index store.
type RM a = RWST (Core.LspFuncs CFG.Config) () I.IndexStore IO a
type MaybeRM a = MaybeT (RWST (Core.LspFuncs CFG.Config) () I.IndexStore IO) a

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs CFG.Config -> TChan ReactorInput -> IO ()
reactor lf rin = do
    debugM "cls.reactor" "Entered"
    
    void $ slipr3 runRWST lf I.emptyStore $ forever $ do
        liftIO $ debugM "cls.reactor" "Reading request"
        hreq <- liftIO $ atomically $ readTChan rin
        liftIO $ debugM "cls.reactor" $ "Got request: " ++ show hreq

        let fileLoader :: C.FileLoader
            fileLoader fp = do
                normUri <- filePathToNormalizedUri fp
                vfile <- Core.getVirtualFileFunc lf normUri
                
                case T.unpack . VFS.virtualFileText <$> vfile of
                    Just vfsContent -> return vfsContent
                    Nothing -> readFile fp

        case hreq of
            NotInitialized _ -> do
                liftIO $ setupLogging $ Core.sendFunc lf
                liftIO $ infoM "cls.reactor" "Initialized, building index store..."
                folders <- liftIO $ ((maybeToList . folderToPath) =<<) <$> (fromMaybe [] <$> Core.getWorkspaceFolders lf)
                runMaybeT $ sequence $ addDirToIndexStore fileLoader <$> folders
                count <- I.getModuleCount
                liftIO $ infoM "cls.reactor" $ "Indexed " ++ show count ++ " files"
                where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

            RspFromClient rsp -> do
                liftIO $ debugM "cls.reactor" $ "Response from client: " ++ show rsp
            
            NotDidChangeConfiguration _ -> void $ runMaybeT $ do
                cfg <- getConfig
                let rawLevel = CFG.logLevel cfg
                    level = parseLogLevel rawLevel
                liftIO $ infoM "cls.reactor" $ "Changed configuration: " ++ show cfg
                liftIO $ case level of
                    Just l -> do
                        updateLogLevel l
                        infoM "cls.reactor" $ "Updated log level to " ++ rawLevel
                    Nothing -> infoM "cls.reactor" $ "Could not parse log level " ++ rawLevel

            NotDidOpenTextDocument notification -> do
                liftIO $ debugM "cls.reactor" "Processing open notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore fileLoader uri
            
            NotDidChangeTextDocument notification -> do
                liftIO $ debugM "cls.reactor" "Processing change notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore fileLoader uri

            NotDidSaveTextDocument notification -> (do
                liftIO $ debugM "cls.reactor" "Processing save notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore fileLoader uri) :: RM ()
            
            ReqCompletion req -> do
                liftIO $ debugM "cls.reactor" "Processing completion request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                completions <- fmap (join . maybeToList) $ runMaybeT $ do
                    entry <- I.getModule normUri
                    vfile <- liftMaybe =<< liftIO (Core.getVirtualFileFunc lf normUri)
                    query <- liftMaybe $ wordAtPos pos $ VFS.virtualFileText vfile
                    liftIO $ fetchCompletions entry query pos
                let maxCompletions = 25
                    items = take maxCompletions completions
                    incomplete = length completions > maxCompletions
                    result = J.CompletionList $ J.CompletionListType incomplete $ J.List items
                send $ RspCompletion $ Core.makeResponseMessage req result
            
            ReqCompletionItemResolve req -> do
                liftIO $ debugM "cls.reactor" "Processing completion item resolve request"
                let item = req ^. J.params
                -- TODO
                send $ RspCompletionItemResolve $ Core.makeResponseMessage req item

            ReqHover req -> do
                liftIO $ debugM "cls.reactor" "Processing hover request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                hover <- runMaybeT $ do
                    entry <- I.getModule normUri
                    liftMaybe =<< liftIO (fetchHover entry pos)
                send $ RspHover $ Core.makeResponseMessage req hover
            
            ReqDefinition req -> do
                liftIO $ debugM "cls.reactor" "Processing definition request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                defs <- runMaybeT $ do
                    liftIO $ debugM "cls.reactor" $ "Looking up " ++ show normUri ++ " in " ++ show (M.keys $ I.modules store)
                    entry <- I.getModule normUri
                    liftIO $ fetchDefinitions store entry pos
                send $ RspDefinition $ Core.makeResponseMessage req $ case defs of Just [d] -> J.SingleLoc d
                                                                                   Just ds  -> J.MultiLoc ds
                                                                                   Nothing  -> J.MultiLoc []

            ReqDocumentSymbols req -> do
                liftIO $ debugM "cls.reactor" "Processing document symbols request"
                let uri = req ^. J.params . J.textDocument . J.uri
                normUri <- liftIO $ normalizeUriWithPath uri
                symbols <- runMaybeT $ do
                    entry <- I.getModule normUri
                    liftIO $ fetchDocumentSymbols entry
                send $ RspDocumentSymbols $ Core.makeResponseMessage req $ fromMaybe (J.DSDocumentSymbols $ J.List []) symbols
            
            ReqWorkspaceSymbols req -> do
                liftIO $ debugM "cls.reactor" "Processing workspace symbols request"
                let query = req ^. J.params . J.query
                store <- get
                symbols <- liftIO $ fetchWorkspaceSymbols store query
                send $ RspWorkspaceSymbols $ Core.makeResponseMessage req $ J.List symbols

            req -> do
                liftIO $ noticeM "cls.reactor" $ "Got unrecognized request: " ++ show req
        
        liftIO $ debugM "cls.reactor" "Handled request"

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: C.FileLoader -> FilePath -> MaybeRM ()
addDirToIndexStore fl dirPath = do
    cfg <- getConfig
    I.addWorkspaceDir cfg fl dirPath
    entries <- I.getModuleList
    void $ lift $ sequence $ (uncurry sendDiagnostics <=< withUriEntry2Diags) <$> entries
    where withUriEntry2Diags :: (J.NormalizedUri, I.ModuleStoreEntry) -> RM (J.NormalizedUri, [J.Diagnostic])
          withUriEntry2Diags (uri, entry) = (uri,) <$> liftIO (fetchDiagnostics uri entry)

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: C.FileLoader -> J.Uri -> MaybeRM ()
updateIndexStore fl uri = do
    cfg <- getConfig
    normUri <- liftIO $ normalizeUriWithPath uri
    lift $ I.recompileModule cfg fl normUri
    entry <- I.getModule normUri
    diags <- liftIO $ fetchDiagnostics normUri entry
    lift $ sendDiagnostics normUri diags

-- | Sends an LSP message to the client.
send :: FromServerMessage -> RM ()
send msg = do
    lf <- ask
    liftIO $ Core.sendFunc lf msg

-- | Publishes diagnostic messages in the given source file to the client.
sendDiagnostics :: J.NormalizedUri -> [J.Diagnostic] -> RM ()
sendDiagnostics uri diags = do
    lf <- ask
    liftIO $ Core.publishDiagnosticsFunc lf maxToPublish uri version diagsBySource
    where version = Just 0
          maxToPublish = 100
          -- Workaround for empty diagnostics: https://github.com/alanz/haskell-lsp/issues/139
          diagsBySource | null diags = M.singleton Nothing (SL.toSortedList [])
                        | otherwise  = partitionBySource diags

-- | Fetches the configuration 
getConfig :: MaybeRM CFG.Config
getConfig = do
    lf <- ask
    liftMaybe =<< liftIO (Core.config lf)
