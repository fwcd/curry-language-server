{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Diagnostics (emitDiagnostics, fetchDiagnostics) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Index.Store (ModuleStoreEntry (..))
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.Uri (normalizedUriToFilePath)
import Curry.LanguageServer.Monad
import qualified Data.Map as M
import qualified Data.SortedList as SL
import qualified Language.LSP.Diagnostics as D
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.FilePath (takeBaseName)
import System.Log.Logger

emitDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> LSM ()
emitDiagnostics normUri entry = do
    diags <- liftIO $ fetchDiagnostics normUri entry
    let -- Workaround for empty diagnostics: https://github.com/alanz/haskell-lsp/issues/139
        diagsBySrc | null diags = M.singleton Nothing (SL.toSortedList [])
                   | otherwise  = D.partitionBySource diags
        maxDiags = 500
        version = Just 0
    S.publishDiagnostics maxDiags normUri version diagsBySrc

fetchDiagnostics :: J.NormalizedUri -> ModuleStoreEntry -> IO [J.Diagnostic]
fetchDiagnostics normUri entry = do
    let warnings = map (curryMsg2Diagnostic J.DsWarning) $ mseWarningMessages entry
        errors = map (curryMsg2Diagnostic J.DsError) $ mseErrorMessages entry
        diags = warnings ++ errors
        name = maybe "?" takeBaseName $ normalizedUriToFilePath normUri
    
    unless (null diags) $
        infoM "cls.diagnostics" $ "Found " ++ show (length diags) ++ " message(s) in " ++ name

    return diags
