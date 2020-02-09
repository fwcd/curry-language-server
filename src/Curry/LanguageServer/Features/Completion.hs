{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Completion (fetchCompletions) where

import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import qualified Language.Haskell.LSP.Types as J

fetchCompletions :: IndexStoreEntry -> J.Position -> IO [J.CompletionItem]
fetchCompletions entry pos = do
    let completions = [J.CompletionItem "Test" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing] -- TODO
    logs INFO $ "fetchCompletions: Found " ++ show completions
    return completions
