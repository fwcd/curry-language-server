{-# LANGUAGE LambdaCase, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Handlers
import Curry.LanguageServer.Handlers.Initialize (initializeHandler)
import Curry.LanguageServer.Handlers.Workspace.Command (commands)
import Curry.LanguageServer.Monad (runLSM, newLSStateVar)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (stdin, stdout)

main :: IO ()
main = runLanguageServer >>= \case
    0 -> exitSuccess
    c -> exitWith $ ExitFailure c

runLanguageServer :: IO Int
runLanguageServer = do
    state <- newLSStateVar
    S.runServerWithHandles logger logger stdin stdout $ S.ServerDefinition
        { S.defaultConfig = def
        , S.parseConfig = \_old v -> case A.fromJSON v of
                                            A.Error e -> Left $ T.pack e
                                            A.Success cfg -> Right (cfg :: CFG.Config)
        , S.configSection = "curry"
        -- TODO: Handle configuration changes (ideally from here, not in the didChangeConfiguration handler)
        -- See https://hackage.haskell.org/package/lsp-2.7.0.0/docs/Language-LSP-Server.html#t:ServerDefinition
        , S.onConfigChange = const $ pure ()
        , S.doInitialize = \env req -> runLSM (initializeHandler req) state env >> return (Right env)
        , S.staticHandlers = handlers
        , S.interpretHandler = \env -> S.Iso (\lsm -> runLSM lsm state env) liftIO
        , S.options = S.defaultOptions
            { S.optTextDocumentSync = Just syncOptions
            , S.optCompletionTriggerCharacters = Just ['.']
            , S.optSignatureHelpTriggerCharacters = Just [' ', '(', ')']
            , S.optExecuteCommandCommands = Just $ fst <$> commands
            , S.optServerInfo = Just $ J.ServerInfo "Curry Language Server" Nothing
            }
        }
    where
        -- We discard log messages originating from the LSP framework for now,
        -- since logging every JSON-RPC message makes the output hard to read.
        -- Eventually we may want to filter by severity (e.g. >= Info) or define
        -- our own `pretty :: LspServerLog -> Text`, similar to
        -- https://github.com/haskell/lsp/blob/7c1fcaa1073dc79e6b330b06e34d30b5d0045af6/lsp/src/Language/LSP/Server/Control.hs#L56-L71
        -- and filter out all the cases that we do not care about (e.g. ParsedMsg, SendMsg).
        logger :: Monad m => LogAction m (WithSeverity S.LspServerLog)
        logger = LogAction $ const $ return ()
        syncOptions = J.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just J.TextDocumentSyncKind_Incremental) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ J.InR $ J.SaveOptions $ Just False) -- save
