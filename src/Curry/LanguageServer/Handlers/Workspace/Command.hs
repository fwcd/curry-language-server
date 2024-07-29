{-# LANGUAGE DataKinds, OverloadedStrings, ViewPatterns, TypeOperators #-}
module Curry.LanguageServer.Handlers.Workspace.Command (executeCommandHandler, commands) where

import Control.Lens ((^.))
import Control.Monad (void)
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J

executeCommandHandler :: S.Handlers LSM
executeCommandHandler = S.requestHandler J.SMethod_WorkspaceExecuteCommand $ \req responder -> do
    debugM "Processing command execution request"
    let J.ExecuteCommandParams _ name args = req ^. J.params
    res <- executeCommand name $ fromMaybe [] args
    responder res

executeCommand :: T.Text -> [A.Value] -> LSM (Either (J.TResponseError J.Method_WorkspaceExecuteCommand) (A.Value J.|? J.Null))
executeCommand name args = case lookup name commands of
    Just command -> command args
    Nothing -> do
        let msg = "Unknown command '" <> name <> "'"
        return $ Left $ J.TResponseError (J.InR J.ErrorCodes_InvalidParams) msg Nothing

commands :: [(T.Text, [A.Value] -> LSM (Either (J.TResponseError J.Method_WorkspaceExecuteCommand) (A.Value J.|? J.Null)))]
commands =
    [ ("ping", \_args -> do
        infoM "Pong!"
        return $ Right $ J.InR J.Null)
    , ("decl.applyTypeHint", \args -> do
        case args of
            [A.fromJSON -> A.Success uri, A.fromJSON -> A.Success pos, A.fromJSON -> A.Success text] -> do
                let doc = J.OptionalVersionedTextDocumentIdentifier uri $ J.InL 0
                    range = J.Range pos pos
                    textEdit = J.TextEdit range $ text <> "\n"
                    docEdit = J.TextDocumentEdit doc [J.InL textEdit]
                    docEdits = [docEdit]
                    workspaceEdit = J.WorkspaceEdit Nothing (Just $ J.InL <$> docEdits) Nothing
                    params = J.ApplyWorkspaceEditParams (Just "Apply Type Hint") workspaceEdit
                void $ S.sendRequest J.SMethod_WorkspaceApplyEdit params (const $ pure ())
                return $ Right $ J.InR J.Null
            _ -> return $ Left $ J.TResponseError (J.InR J.ErrorCodes_InvalidParams) "Invalid arguments!" Nothing)
    ]
