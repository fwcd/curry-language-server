{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics, fetchDiagnostics) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Index.Store (ModuleStoreEntry (..))
import Curry.LanguageServer.Utils.Convert (curryMsg2Diagnostic)
import Curry.LanguageServer.Utils.Uri (normalizedUriToFilePath)
import Curry.LanguageServer.Utils.Logging (infoM)
import Curry.LanguageServer.Monad (LSM)
import qualified Data.Map as M
import qualified Data.SortedList as SL
import qualified Data.Text as T
import qualified Language.LSP.Diagnostics as D
import qualified Language.LSP.Server as S
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Protocol.Types as J
import System.FilePath (takeBaseName)

emitDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> LSM ()
emitDiagnostics normUri entry = do
    diags <- fetchDiagnostics normUri entry
    let -- Workaround for empty diagnostics: https://github.com/haskell/lsp/issues/139
        diagsBySrc | null diags = M.singleton Nothing (SL.toSortedList [])
                   | otherwise  = D.partitionBySource diags
        maxDiags = 500
        version = Just 0
    S.publishDiagnostics maxDiags normUri version diagsBySrc

fetchDiagnostics :: (MonadIO m, MonadLsp CFG.Config m) => J.NormalizedUri -> ModuleStoreEntry -> m [J.Diagnostic]
fetchDiagnostics normUri entry = do
    let warnings = map (curryMsg2Diagnostic J.DiagnosticSeverity_Warning) entry.warningMessages
        errors = map (curryMsg2Diagnostic J.DiagnosticSeverity_Error) entry.errorMessages
        diags = warnings ++ errors
        name = maybe "?" takeBaseName $ normalizedUriToFilePath normUri
    
    unless (null diags) $
        infoM $ "Found " <> T.pack (show (length diags)) <> " message(s) in " <> T.pack name

    return diags
