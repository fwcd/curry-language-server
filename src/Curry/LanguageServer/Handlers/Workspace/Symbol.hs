{-# LANGUAGE NoFieldSelectors, OverloadedStrings, OverloadedRecordDot, FlexibleContexts #-}
module Curry.LanguageServer.Handlers.Workspace.Symbol (workspaceSymbolHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Curry.LanguageServer.Config as CFG
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Monad (LSM, getStore)
import Curry.LanguageServer.Utils.Logging (debugM, infoM)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J
import Language.LSP.Server (MonadLsp)

workspaceSymbolHandler :: S.Handlers LSM
workspaceSymbolHandler = S.requestHandler J.SMethod_WorkspaceSymbol $ \req responder -> do
    debugM "Processing workspace symbols request"
    let query = req ^. J.params . J.query
    store <- getStore
    symbols <- fetchWorkspaceSymbols store query
    let maxSymbols = 150
    responder $ Right $ J.InL $ take maxSymbols symbols

fetchWorkspaceSymbols :: (MonadIO m, MonadLsp CFG.Config m) => I.IndexStore -> T.Text -> m [J.SymbolInformation]
fetchWorkspaceSymbols store query = do
    debugM $ "Searching " <> T.pack (show (I.storedSymbolCount store)) <> " symbol(s)..."
    let symbols = mapMaybe toWorkspaceSymbol $ I.storedSymbolsWithPrefix query store
    infoM $ "Found " <> T.pack (show (length symbols)) <> " symbol(s)"
    return symbols

toWorkspaceSymbol :: I.Symbol -> Maybe J.SymbolInformation
toWorkspaceSymbol s = J.SymbolInformation name kind tags containerName deprecated <$> s.location
    where name = s.ident
          kind = case s.kind of
              I.ValueFunction    | s.arrowArity == Just 0 -> J.SymbolKind_Constant
                                 | otherwise              -> J.SymbolKind_Function
              I.ValueConstructor | s.arrowArity == Just 0 -> J.SymbolKind_EnumMember
                                 | otherwise              -> J.SymbolKind_Constructor
              I.Module                                    -> J.SymbolKind_Module
              I.TypeData | length s.constructors == 1     -> J.SymbolKind_Struct
                         | otherwise                      -> J.SymbolKind_Enum
              I.TypeNew                                   -> J.SymbolKind_Struct
              I.TypeAlias                                 -> J.SymbolKind_Interface
              I.TypeClass                                 -> J.SymbolKind_Interface
              I.TypeVar                                   -> J.SymbolKind_Variable
              I.Other                                     -> J.SymbolKind_Namespace
          tags = Nothing
          deprecated = Nothing
          containerName = Just $ I.symbolParentIdent s
