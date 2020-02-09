module Curry.LanguageServer.Reactor (reactor, ReactorInput (..)) where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Features.Definition
import Curry.LanguageServer.Features.Diagnostics
import Curry.LanguageServer.Features.DocumentSymbols
import Curry.LanguageServer.Features.Hover
import Curry.LanguageServer.Features.WorkspaceSymbols
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General (liftMaybe, slipr3)
import Curry.LanguageServer.Utils.Uri (normalizeUriWithPath)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The input to the reactor.
newtype ReactorInput = HandlerRequest FromClientMessage

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
        liftIO $ logs DEBUG "Reading request"
        hreq <- liftIO $ atomically $ readTChan rin

        case hreq of
            HandlerRequest (NotInitialized _) -> do
                let logLevel = INFO
                liftIO $ setupLogging (Core.sendFunc lf) logLevel
                liftIO $ logs INFO $ "reactor: Initialized, building index store..."
                folders <- liftIO $ ((maybeToList . folderToPath) =<<) <$> (maybe [] id <$> Core.getWorkspaceFolders lf)
                runMaybeT $ sequence $ addDirToIndexStore <$> folders
                count <- I.getCount
                liftIO $ logs INFO $ "reactor: Indexed " ++ show count ++ " files"
                where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

            HandlerRequest (RspFromClient rsp) -> do
                liftIO $ logs DEBUG $ "reactor: Response from client: " ++ show rsp
            
            HandlerRequest (NotDidChangeConfiguration notification) -> void $ runMaybeT $ do
                cfg <- getConfig
                liftIO $ logs INFO $ "reactor: Changed configuration: " ++ show cfg

            HandlerRequest (NotDidOpenTextDocument notification) -> do
                liftIO $ logs DEBUG $ "reactor: Processing open notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore uri
            
            -- TODO: Respond to changes before saving (possibly requires using the VFS)

            HandlerRequest (NotDidSaveTextDocument notification) -> (do
                liftIO $ logs DEBUG $ "reactor: Processing save notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                void $ runMaybeT $ updateIndexStore uri) :: RM ()
                
            HandlerRequest (ReqHover req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing hover request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                hover <- runMaybeT $ do
                    entry <- I.getEntry normUri
                    liftMaybe =<< (liftIO $ fetchHover entry pos)
                send $ RspHover $ Core.makeResponseMessage req hover
            
            HandlerRequest (ReqDefinition req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing definition request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                defs <- runMaybeT $ do
                    liftIO $ logs INFO $ "Looking up " ++ show normUri ++ " in " ++ show (M.keys store)
                    entry <- I.getEntry normUri
                    liftIO $ fetchDefinitions store entry pos
                send $ RspDefinition $ Core.makeResponseMessage req $ case defs of Just [d] -> J.SingleLoc d
                                                                                   Just ds  -> J.MultiLoc ds
                                                                                   Nothing  -> J.MultiLoc []

            HandlerRequest (ReqDocumentSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing document symbols request"
                let uri = req ^. J.params . J.textDocument . J.uri
                normUri <- liftIO $ normalizeUriWithPath uri
                store <- get
                symbols <- runMaybeT $ do
                    entry <- I.getEntry normUri
                    liftIO $ fetchDocumentSymbols entry
                send $ RspDocumentSymbols $ Core.makeResponseMessage req $ maybe (J.DSDocumentSymbols $ J.List []) id symbols
            
            HandlerRequest (ReqWorkspaceSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing workspace symbols request"
                let query = req ^. J.params . J.query
                store <- get
                symbols <- liftIO $ fetchWorkspaceSymbols query store
                send $ RspWorkspaceSymbols $ Core.makeResponseMessage req $ J.List symbols

            HandlerRequest req -> do
                liftIO $ logs DEBUG $ "reactor: Other HandlerRequest: " ++ show req

-- | Indexes a workspace folder recursively.
addDirToIndexStore :: FilePath -> MaybeRM ()
addDirToIndexStore dirPath = do
    cfg <- getConfig
    I.addWorkspaceDir cfg dirPath
    entries <- I.getEntries
    void $ lift $ sequence $ (uncurry sendDiagnostics =<<) <$> withUriEntry2Diags <$> entries
    where withUriEntry2Diags :: (J.NormalizedUri, I.IndexStoreEntry) -> RM (J.NormalizedUri, [J.Diagnostic])
          withUriEntry2Diags (uri, entry) = (\ds -> (uri, ds)) <$> (liftIO $ fetchDiagnostics entry)

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> MaybeRM ()
updateIndexStore uri = do
    cfg <- getConfig
    normUri <- liftIO $ normalizeUriWithPath uri
    lift $ I.recompileEntry cfg normUri
    entry <- I.getEntry normUri
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
    liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri version $ partitionBySource diags
    where version = Just 0
          maxToPublish = 100

-- | Fetches the configuration 
getConfig :: MaybeRM CFG.Config
getConfig = do
    lf <- ask
    liftMaybe =<< (liftIO $ Core.config lf)
