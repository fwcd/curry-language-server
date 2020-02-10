module Curry.LanguageServer.Options (lspOptions) where

import Data.Default
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J

lspOptions :: Core.Options
lspOptions = def { Core.textDocumentSync = Just syncOptions }
                --    Core.completionTriggerCharacters = Just "." }

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions {
        J._openClose = Just True,
        J._change = Just J.TdSyncIncremental,
        J._willSave = Just False,
        J._willSaveWaitUntil = Just False,
        J._save = Just $ J.SaveOptions $ Just False
    }
