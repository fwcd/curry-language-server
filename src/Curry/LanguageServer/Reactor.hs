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
import Data.Default
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types as J
import Language.Haskell.LSP.Types.Lens as J
import Language.Haskell.LSP.Utility as U

-- Based on https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs (MIT-licensed, Copyright (c) 2016 Alan Zimmerman)

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides:
-- Language server and compiler frontend
reactor :: Core.LspFuncs C.Config -> TChan ReactorInput -> IO ()
reactor lf rin = do
    U.logs "reactor: entered"
    flip runReaderT lf $ forever $ do
        hreq <- liftIO $ atomically $ readTChan rin
        config <- maybe def Prelude.id <$> (liftIO $ Core.config lf)

        case hreq of
            HandlerRequest (RspFromClient rsp) -> do
                liftIO $ U.logs $ "reactor: RspFromClient " ++ show rsp
            
            HandlerRequest (NotDidOpenTextDocument notification) -> do
                liftIO $ U.logs $ "reactor: Processing NotDidOpenTextDocument"
                let uri = notification ^. J.params . J.textDocument . J.uri
                    version = Just 0
                compilation <- liftIO $ compileCurryFromUri config uri
                diags <- liftIO $ fetchDiagnostics compilation
                sendDiagnostics 100 uri version $ partitionBySource diags
            
            -- TODO: Respond to changes before saving (possibly requires using the VFS)

            HandlerRequest (NotDidSaveTextDocument notification) -> do
                liftIO $ U.logs $ "reactor: Processing NotDidSaveTextDocument"
                let uri = notification ^. J.params . J.textDocument . J.uri
                    version = Just 0
                compilation <- liftIO $ compileCurryFromUri config uri
                diags <- liftIO $ fetchDiagnostics compilation
                sendDiagnostics 100 uri version $ partitionBySource diags

            HandlerRequest req -> do
                liftIO $ U.logs $ "reactor: Other HandlerRequest " ++ show req

compileCurryFromUri :: C.Config -> J.Uri -> IO ConcreteCompilationResult
compileCurryFromUri config uri = maybe failed (compileCurry importPaths) optFilePath
    where importPaths = C.importPaths config
          optFilePath = J.uriToFilePath uri
          failed = return $ failedCompilation "Language Server: Cannot construct file path from URI"

sendDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> RM ()
sendDiagnostics maxToPublish uri v diags = do
    lf <- ask
    liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish (J.toNormalizedUri uri) v diags
