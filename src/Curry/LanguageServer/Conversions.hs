{-# LANGUAGE RecordWildCards #-}
module Curry.LanguageServer.Conversions where

-- Curry Compiler Libraries + Dependencies
import Curry.Base.Message as CM
import Curry.Base.Position as CP
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Text.PrettyPrint as PP

import Data.Maybe
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import System.FilePath

-- Curry Compiler -> Language Server Protocol

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

currySpanInfo2Range :: CSPI.SpanInfo -> Maybe J.Range
currySpanInfo2Range CSPI.NoSpanInfo = Nothing
currySpanInfo2Range CSPI.SpanInfo {..} = currySpan2Range srcSpan

currySpan2Range :: CSP.Span -> Maybe J.Range
currySpan2Range CSP.NoSpan = Nothing
currySpan2Range CSP.Span {..} = do
    s <- curryPos2Pos start
    e <- curryPos2Pos end
    return $ J.Range s e

-- Language Server Protocol -> Curry Compiler

-- TODO
