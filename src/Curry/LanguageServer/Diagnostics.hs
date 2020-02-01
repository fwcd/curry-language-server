{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Diagnostics where

import Curry.LanguageServer.Aliases
import Data.Text
import qualified Language.Haskell.LSP.Types as J

fetchDiagnostics :: J.NormalizedUri -> RM () [J.Diagnostic]
fetchDiagnostics = const $ return [
        J.Diagnostic (J.Range (J.Position 0 0) (J.Position 0 1)) Nothing Nothing Nothing "Test" Nothing
    ] -- TODO
