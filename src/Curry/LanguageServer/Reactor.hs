module Curry.LanguageServer.Reactor (reactor, ReactorInput (..)) where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Features.Completion
import Curry.LanguageServer.Features.Definition
import Curry.LanguageServer.Features.Diagnostics
import Curry.LanguageServer.Features.DocumentSymbols
import Curry.LanguageServer.Features.Hover
import Curry.LanguageServer.Features.WorkspaceSymbols
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General (liftMaybe, slipr3, wordAtPos)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.SortedList as SL
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.VFS as VFS
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

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
    logs DEBUG "reactor: entered"
    
    void $ slipr3 runRWST lf I.emptyStore $ forever $ do
        liftIO $ logs DEBUG "reactor: Reading request"
        hreq <- liftIO $ atomically $ readTChan rin
        liftIO $ logs DEBUG $ "reactor: Got request: " ++ show hreq

        case hreq of
            NotInitialized _ -> do
                liftIO $ setupLogging $ Core.sendFunc lf
                liftIO $ logs INFO $ "reactor: Initialized, building index store..."
                folders <- liftIO $ ((maybeToList . folderToPath) =<<) <$> (maybe [] id <$> Core.getWorkspaceFolders lf)
                runMaybeT $ sequence $ addDirToIndexStore <$> folders
                count <- I.getModuleCount
                liftIO $ logs INFO $ "reactor: Indexed " ++ show count ++ " files"
                where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

            RspFromClient rsp -> do
                liftIO $ logs DEBUG $ "reactor: Response from client: " ++ show rsp
            
            NotDidChangeConfiguration notification -> void $ runMaybeT $ do
                cfg <- getConfig
                let rawLevel = CFG.logLevel cfg
                    level = parseLogLevel rawLevel
                liftIO $ logs INFO $ "reactor: Changed configuration: " ++ show cfg
                liftIO $ case level of
                    Just l -> do
                        updateLogLevel l
                        logs INFO $ "Updated log level to " ++ rawLevel
                    Nothing -> logs INFO $ "reactor: Could not parse log level " ++ rawLevel

            NotDidOpenTextDocument notification -> do
                liftIO $ logs DEBUG $ "reactor: Processing open notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore uri
            
            -- TODO: Respond to changes before saving (possibly requires using the VFS)

            NotDidSaveTextDocument notification -> (do
                liftIO $ logs DEBUG $ "reactor: Processing save notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore uri) :: RM ()
            
            ReqCompletion req -> do
                liftIO $ logs DEBUG $ "reactor: Processing completion request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                vfile <- liftIO $ Core.getVirtualFileFunc lf normUri
                completions <- fmap (join . maybeToList) $ runMaybeT $ do
                    entry <- I.getModule normUri
                    vfile <- liftMaybe =<< (liftIO $ Core.getVirtualFileFunc lf normUri)
                    query <- liftMaybe $ wordAtPos pos $ VFS.virtualFileText vfile
                    liftIO $ fetchCompletions entry query pos
                let maxCompletions = 25
                    items = take maxCompletions completions
                    incomplete = length completions > maxCompletions
                    result = J.CompletionList $ J.CompletionListType incomplete $ J.List items
                send $ RspCompletion $ Core.makeResponseMessage req result
            
            ReqCompletionItemResolve req -> do
                liftIO $ logs DEBUG $ "reactor: Processing completion item resolve request"
                let item = req ^. J.params
                -- TODO
                send $ RspCompletionItemResolve $ Core.makeResponseMessage req item

            ReqHover req -> do
                liftIO $ logs DEBUG $ "reactor: Processing hover request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                hover <- runMaybeT $ do
                    entry <- I.getModule normUri
                    liftMaybe =<< (liftIO $ fetchHover entry pos)
                send $ RspHover $ Core.makeResponseMessage req hover
            
            ReqDefinition req -> do
                liftIO $ logs DEBUG $ "reactor: Processing definition request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                defs <- runMaybeT $ do
                    liftIO $ logs INFO $ "Looking up " ++ show normUri ++ " in " ++ show (M.keys $ I.modules store)
                    entry <- I.getModule normUri
                    liftIO $ fetchDefinitions store entry pos
                send $ RspDefinition $ Core.makeResponseMessage req $ case defs of Just [d] -> J.SingleLoc d
                                                                                   Just ds  -> J.MultiLoc ds
                                                                                   Nothing  -> J.MultiLoc []

            ReqDocumentSymbols req -> do
                liftIO $ logs DEBUG $ "reactor: Processing document symbols request"
                let uri = req ^. J.params . J.textDocument . J.uri
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                symbols <- runMaybeT $ do
                    entry <- I.getModule normUri
                    liftIO $ fetchDocumentSymbols entry
                send $ RspDocumentSymbols $ Core.makeResponseMessage req $ maybe (J.DSDocumentSymbols $ J.List []) id symbols
            
            ReqWorkspaceSymbols req -> do
                liftIO $ logs DEBUG $ "reactor: Processing workspace symbols request"
                let query = req ^. J.params . J.query
                store <- get
                symbols <- liftIO $ fetchWorkspaceSymbols store query
                send $ RspWorkspaceSymbols $ Core.makeResponseMessage req $ J.List symbols

            req -> do
                liftIO $ logs NOTICE $ "reactor: Got unrecognized request: " ++ show req
        
        liftIO $ logs DEBUG $ "reactor: Handled request"

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: FilePath -> MaybeRM ()
addDirToIndexStore dirPath = do
    cfg <- getConfig
    I.addWorkspaceDir cfg dirPath
    entries <- I.getModuleList
    void $ lift $ sequence $ (uncurry sendDiagnostics =<<) <$> withUriEntry2Diags <$> entries
    where withUriEntry2Diags :: (J.NormalizedUri, I.ModuleStoreEntry) -> RM (J.NormalizedUri, [J.Diagnostic])
          withUriEntry2Diags (uri, entry) = (\ds -> (uri, ds)) <$> (liftIO $ fetchDiagnostics entry)

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> MaybeRM ()
updateIndexStore uri = do
    cfg <- getConfig
    normUri <- liftIO $ normalizeUriWithPath uri
    lift $ I.recompileModule cfg normUri
    entry <- I.getModule normUri
    diags <- liftIO $ fetchDiagnostics entry
    lift $ sendDiagnostics normUri diags
    where version = Just 0

-- | Sends an LSP message to the client.
send :: FromServerMessage -> RM ()
send msg = do
    lf <- ask
    liftIO $ Core.sendFunc lf msg

-- | Publishes diagnostic messages in the given source file to the client.
sendDiagnostics :: J.NormalizedUri -> [J.Diagnostic] -> RM ()
sendDiagnostics uri diags = do
    lf <- ask
    liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri version diagsBySource
    where version = Just 0
          maxToPublish = 100
          -- Workaround for empty diagnostics: https://github.com/alanz/haskell-lsp/issues/139
          diagsBySource | null diags = M.singleton Nothing (SL.toSortedList [])
                        | otherwise  = partitionBySource diags

-- | Fetches the configuration 
getConfig :: MaybeRM CFG.Config
getConfig = do
    lf <- ask
    liftMaybe =<< (liftIO $ Core.config lf)
