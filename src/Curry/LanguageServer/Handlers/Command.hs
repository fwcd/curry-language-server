{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Command (commandHandler, commands) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Monad
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

commandHandler :: S.Handlers LSM
commandHandler = S.requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
    liftIO $ debugM "cls.command" "Processing command execution request"
    let J.ExecuteCommandParams _ name args = req ^. J.params
    res <- executeCommand name $ maybe [] (\(J.List as) -> as) args
    responder res

executeCommand :: T.Text -> [A.Value] -> LSM (Either J.ResponseError A.Value)
executeCommand name args = case lookup name commands of
    Just command -> command args
    Nothing -> do
        let msg = "Unknown command '" <> name <> "'"
        liftIO $ warningM "cls.command" $ T.unpack msg
        return $ Left $ J.ResponseError J.InvalidParams msg Nothing

commands :: [(T.Text, [A.Value] -> LSM (Either J.ResponseError A.Value))]
commands =
    [ ("ping", \_args -> do
        liftIO $ infoM "cls.command" "Pong!"
        return $ Right A.Null)
    , ("decl.applyTypeHint", \args -> do
        case args of
            [rawUri, rawPos, rawText] -> do
                -- TODO: Better error handling
                let A.Success uri = A.fromJSON rawUri
                    A.Success pos = A.fromJSON rawPos
                    A.Success text = A.fromJSON rawText
                    doc = J.VersionedTextDocumentIdentifier uri $ Just 0
                    range = J.Range pos pos
                    textEdit = J.TextEdit range $ text <> "\n"
                    docEdit = J.TextDocumentEdit doc $ J.List [textEdit]
                    docEdits = [docEdit]
                    workspaceEdit = J.WorkspaceEdit Nothing $ Just $ J.List docEdits
                    params = J.ApplyWorkspaceEditParams (Just "Apply Type Hint") workspaceEdit
                void $ S.sendRequest J.SWorkspaceApplyEdit params (const $ pure ())
                return $ Right A.Null
            _ -> return $ Left $ J.ResponseError J.InvalidParams "Too few arguments!" Nothing)
    ]
