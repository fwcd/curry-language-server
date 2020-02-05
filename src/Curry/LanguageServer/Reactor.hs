module Curry.LanguageServer.Reactor (reactor) where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.STM
import Curry.LanguageServer.Aliases
import Curry.LanguageServer.Compiler
import qualified Curry.LanguageServer.Config as C
import Curry.LanguageServer.Features.Diagnostics
import Curry.LanguageServer.Features.DocumentSymbols
import Curry.LanguageServer.Features.Hover
import Curry.LanguageServer.Features.WorkspaceSymbols
import Curry.LanguageServer.Logging
import Data.Default
import Data.Maybe (maybeToList)
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.Utility as U

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs C.Config -> TChan ReactorInput -> IO ()
reactor lf rin = do
    logs DEBUG "reactor: entered"
    flip runReaderT lf $ forever $ do
        hreq <- liftIO $ atomically $ readTChan rin
        config <- maybe def Prelude.id <$> (liftIO $ Core.config lf)

        case hreq of
            HandlerRequest (NotInitialized _) -> do
                let logLevel = INFO
                liftIO $ setupLogging (Core.sendFunc lf) logLevel
                liftIO $ logs INFO $ "reactor: Initialized"

            HandlerRequest (RspFromClient rsp) -> do
                liftIO $ logs DEBUG $ "reactor: Response from client: " ++ show rsp
            
            HandlerRequest (NotDidOpenTextDocument notification) -> do
                liftIO $ logs DEBUG $ "reactor: Processing open notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                    version = Just 0
                compilation <- liftIO $ compileCurryFromUri config uri
                diags <- liftIO $ fetchDiagnostics compilation
                sendDiagnostics 100 uri version $ partitionBySource diags
            
            -- TODO: Respond to changes before saving (possibly requires using the VFS)

            HandlerRequest (NotDidSaveTextDocument notification) -> do
                liftIO $ logs DEBUG $ "reactor: Processing save notification"
                let uri = notification ^. J.params . J.textDocument . J.uri
                    version = Just 0
                compilation <- liftIO $ compileCurryFromUri config uri
                diags <- liftIO $ fetchDiagnostics compilation
                sendDiagnostics 100 uri version $ partitionBySource diags
                
            HandlerRequest (ReqHover req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing hover request"
                let uri = req ^. J.params . J.textDocument . J.uri
                    pos = req ^. J.params . J.position
                compilation <- liftIO $ compileCurryFromUri config uri
                hover <- liftIO $ fetchHover compilation pos
                send $ RspHover $ Core.makeResponseMessage req hover
            
            HandlerRequest (ReqDocumentSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing document symbols request"
                let uri = req ^. J.params . J.textDocument . J.uri
                compilation <- liftIO $ compileCurryFromUri config uri
                symbols <- liftIO $ fetchDocumentSymbols compilation
                send $ RspDocumentSymbols $ Core.makeResponseMessage req symbols
            
            HandlerRequest (ReqWorkspaceSymbols req) -> do
                liftIO $ logs DEBUG $ "reactor: Processing workspace symbols request"
                let query = req ^. J.params . J.query
                folders <- liftIO $ ((maybeToList . folderToPath) =<<) <$> (maybe [] id <$> Core.getWorkspaceFolders lf)
                interfaces <- liftIO $ join <$> (sequence $ findWorkspaceInterfaces <$> folders)
                symbols <- liftIO $ fetchWorkspaceSymbols query interfaces
                send $ RspWorkspaceSymbols $ Core.makeResponseMessage req $ J.List symbols
                where folderToPath (J.WorkspaceFolder uri _) = J.uriToFilePath $ J.Uri uri

            HandlerRequest req -> do
                liftIO $ logs DEBUG $ "reactor: Other HandlerRequest: " ++ show req

compileCurryFromUri :: C.Config -> J.Uri -> IO ConcreteCompilationResult
compileCurryFromUri config uri = maybe failed (compileCurry importPaths) optFilePath
    where importPaths = C.importPaths config
          optFilePath = J.uriToFilePath uri
          failed = return $ failedCompilation "Language Server: Cannot construct file path from URI"

send :: FromServerMessage -> RM ()
send msg = do
    lf <- ask
    liftIO $ Core.sendFunc lf msg

sendDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> RM ()
sendDiagnostics maxToPublish uri v diags = do
    lf <- ask
    liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish (J.toNormalizedUri uri) v diags
