{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Curry.LanguageServer.Handlers.Workspace.Command (executeCommandHandler, commands) where

import Control.Lens ((^.))
import Control.Monad (void)
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Logging (debugM, infoM, warnM)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J

executeCommandHandler :: S.Handlers LSM
executeCommandHandler = S.requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
    debugM "Processing command execution request"
    let J.ExecuteCommandParams _ name args = req ^. J.params
    res <- executeCommand name $ maybe [] (\(J.List as) -> as) args
    responder res

executeCommand :: T.Text -> [A.Value] -> LSM (Either J.ResponseError A.Value)
executeCommand name args = case lookup name commands of
    Just command -> command args
    Nothing -> do
        let msg = "Unknown command '" <> name <> "'"
        warnM msg
        return $ Left $ J.ResponseError J.InvalidParams msg Nothing

commands :: [(T.Text, [A.Value] -> LSM (Either J.ResponseError A.Value))]
commands =
    [ ("ping", \_args -> do
        infoM "Pong!"
        return $ Right A.Null)
    , ("decl.applyTypeHint", \args -> do
        case args of
            [A.fromJSON -> A.Success uri, A.fromJSON -> A.Success pos, A.fromJSON -> A.Success text] -> do
                let doc = J.VersionedTextDocumentIdentifier uri $ Just 0
                    range = J.Range pos pos
                    textEdit = J.TextEdit range $ text <> "\n"
                    docEdit = J.TextDocumentEdit doc $ J.List [J.InL textEdit]
                    docEdits = [docEdit]
                    workspaceEdit = J.WorkspaceEdit Nothing (Just $ J.List $ J.InL <$> docEdits) Nothing
                    params = J.ApplyWorkspaceEditParams (Just "Apply Type Hint") workspaceEdit
                void $ S.sendRequest J.SWorkspaceApplyEdit params (const $ pure ())
                return $ Right A.Null
            _ -> return $ Left $ J.ResponseError J.InvalidParams "Invalid arguments!" Nothing)
    ]
