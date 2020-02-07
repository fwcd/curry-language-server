module Curry.LanguageServer.Reactor (reactor, ReactorInput (..)) where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.Compiler
import qualified Curry.LanguageServer.Config as C
import qualified Curry.LanguageServer.IndexStore as I
import Curry.LanguageServer.Features.Diagnostics
import Curry.LanguageServer.Features.DocumentSymbols
import Curry.LanguageServer.Features.Hover
import Curry.LanguageServer.Features.WorkspaceSymbols
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General (liftMaybe, slipr3)
import Data.Default
import Data.Maybe (maybeToList)
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.Utility as U

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The input to the reactor.
newtype ReactorInput = HandlerRequest FromClientMessage

-- | The reactor monad holding a readable environment of LSP functions (with config)
-- and a state containing the index store.
type RM a = MaybeT (RWST (Core.LspFuncs C.Config) () I.IndexStore IO) a

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs C.Config -> TChan ReactorInput -> IO ()
reactor lf rin = do
    logs DEBUG "reactor: entered"
    
    void $ slipr3 runRWST lf I.emptyStore $ runMaybeT $ forever $ do
        hreq <- liftIO $ atomically $ readTChan rin

        case hreq of
            HandlerRequest (NotInitialized _) -> do
                let logLevel = INFO
                liftIO $ setupLogging (Core.sendFunc lf) logLevel
                liftIO $ logs INFO $ "reactor: Initialized, building index store..."
                folders <- liftIO $ ((maybeToList . folderToPath) =<<) <$> (maybe [] id <$> Core.getWorkspaceFolders lf)
                sequence $ addDirToIndexStore <$> folders
                count <- lift $ I.getCount
                liftIO $ logs INFO $ "reactor: Indexed " ++ show count ++ " files"
                where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

            HandlerRequest (RspFromClient rsp) -> do
                liftIO $ logs DEBUG $ "reactor: Response from client: " ++ show rsp
            
            HandlerRequest (NotDidOpenTextDocument notification) -> do
                liftIO $ logs DEBUG $ "reactor: Processing open notification"
            
            -- TODO: Respond to changes before saving (possibly requires using the VFS)

            HandlerRequest (NotDidSaveTextDocument notification) -> do
                liftIO $ logs DEBUG $ "reactor: Processing save notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                updateIndexStore uri
                
            HandlerRequest (ReqHover req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing hover request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                entry <- I.getEntry $ J.toNormalizedUri uri
                hover <- liftIO $ fetchHover entry pos
                send $ RspHover $ Core.makeResponseMessage req hover
            
            HandlerRequest (ReqDocumentSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing document symbols request"
                let uri = req ^. J.params . J.textDocument . J.uri
                entry <- I.getEntry $ J.toNormalizedUri uri
                symbols <- liftIO $ fetchDocumentSymbols entry
                send $ RspDocumentSymbols $ Core.makeResponseMessage req symbols
            
            HandlerRequest (ReqWorkspaceSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing workspace symbols request"
                let query = req ^. J.params . J.query
                store <- get
                symbols <- liftIO $ fetchWorkspaceSymbols query store
                send $ RspWorkspaceSymbols $ Core.makeResponseMessage req $ J.List symbols

            HandlerRequest req -> do
                liftIO $ logs DEBUG $ "reactor: Other HandlerRequest: " ++ show req

-- | Indexes a folder recursively.
addDirToIndexStore :: FilePath -> RM ()
addDirToIndexStore dirPath = do
    I.compileDirRecursively dirPath
    entries <- I.getEntries
    void $ sequence $ (uncurry sendDiagnostics =<<) <$> withUriEntry2Diags <$> entries
    where withUriEntry2Diags :: (J.NormalizedUri, I.IndexStoreEntry) -> RM (J.NormalizedUri, [J.Diagnostic])
          withUriEntry2Diags (uri, entry) = (\ds -> (uri, ds)) <$> (liftIO $ fetchDiagnostics entry)

-- | Recompiles and stores the updated compilation for a given URI.
updateIndexStore :: J.Uri -> RM ()
updateIndexStore uri = do
    lift $ I.recompileEntry normUri
    entry <- I.getEntry normUri
    diags <- liftIO $ fetchDiagnostics entry
    sendDiagnostics normUri diags
    where normUri = J.toNormalizedUri uri
          version = Just 0

-- | Compiles a curry source file (inside the reactor monad).
compileCurryFromUri :: J.Uri -> RM CompilationResult
compileCurryFromUri uri = do
    lf <- ask
    cfg <- maybe def Prelude.id <$> (liftIO $ Core.config lf)
    liftIO $ maybe failed (compileCurry $ C.importPaths cfg) optFilePath
    where optFilePath = J.uriToFilePath uri
          failed = return $ failedCompilation "Language Server: Cannot construct file path from URI"

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
