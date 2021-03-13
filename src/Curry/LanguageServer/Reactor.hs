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
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
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

        case hreq of
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

            req -> do
                liftIO $ noticeM "cls.reactor" $ "Got unrecognized request: " ++ show req
        
        liftIO $ debugM "cls.reactor" "Handled request"

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
          

-- | Fetches the configuration 
getConfig :: MaybeRM CFG.Config
getConfig = do
    lf <- ask
    liftMaybe =<< liftIO (Core.config lf)
