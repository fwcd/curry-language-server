{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.IndexStore (ModuleStoreEntry (..))
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Uri (normalizedUriToFilePath)
import Curry.LanguageServer.Monad
import qualified Language.LSP.Diagnostics as D
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.FilePath (takeBaseName)
import System.Log.Logger

emitDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> LSM ()
emitDiagnostics normUri entry = do
    diags <- liftIO $ fetchDiagnostics normUri entry
    let diagSrc = "curry"
        diagsBySrc = D.partitionBySource diags
        maxDiags = 500
    if null diags
        then S.flushDiagnosticsBySource maxDiags $ Just diagSrc
        else S.publishDiagnostics maxDiags normUri (Just 0) diagsBySrc

fetchDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> IO [J.Diagnostic]
fetchDiagnostics normUri entry = do
    let warnings = map (curryMsg2Diagnostic J.DsWarning) $ warningMessages entry
        errors = map (curryMsg2Diagnostic J.DsError) $ errorMessages entry
        diags = warnings ++ errors
        name = maybe "?" takeBaseName $ normalizedUriToFilePath normUri
    
    unless (null diags) $
        infoM "cls.fetchDiagnostics" $ "Found " ++ show (length diags) ++ " message(s) in " ++ name

    return diags
