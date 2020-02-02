{-# LANGUAGE RecordWildCards #-}
module Curry.LanguageServer.Diagnostics (fetchDiagnostics) where

-- Curry Compiler Libraries + Dependencies
import Curry.Base.Message as CM
import Curry.Base.Position as CP
import qualified Text.PrettyPrint as PP

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Curry.LanguageServer.Compiler
import Data.Maybe
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U
import System.FilePath

fetchDiagnostics :: [FilePath] -> J.Uri -> IO [J.Diagnostic]
fetchDiagnostics importPaths doc = do
    U.logs $ "fetchDiagnostics: Import paths: " ++ show importPaths
    msgs <- maybe (return $ Left []) (compileCurry importPaths) $ J.uriToFilePath doc
    let diags = case (msgs :: Either [CM.Message] ((), [CM.Message])) of
                    Left errs -> map (curryMsg2Diagnostic J.DsError) errs
                    Right (_, warns) -> map (curryMsg2Diagnostic J.DsWarning) warns
    U.logs $ "fetchDiagnostics: Found " ++ show diags
    return diags

curryMsg2Diagnostic :: J.DiagnosticSeverity -> CM.Message -> J.Diagnostic
curryMsg2Diagnostic s msg = J.Diagnostic range severity code src text related
    where pos@(J.Position ln col) = maybe (J.Position 0 0) id $ curryPos2Pos =<< CM.msgPos msg
          -- TODO: Fetch a span from Curry compiler instead of just a position
          range = J.Range pos $ J.Position ln (col + 200)
          severity = Just s
          code = Nothing
          src = Nothing
          text = T.pack $ PP.render $ CM.msgTxt msg
          related = Nothing

-- TODO: Use (file :: FilePath) from Curry Position to accurately
--       map diagnostics to their respective files.
curryPos2Pos :: CP.Position -> Maybe J.Position
curryPos2Pos CP.NoPos = Nothing
curryPos2Pos CP.Position {..} = Just $ J.Position (line - 1) (column - 1)
