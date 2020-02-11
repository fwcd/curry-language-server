module Curry.LanguageServer.Features.Completion (fetchCompletions) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CT
import qualified CompilerEnv as CE
import qualified Env.Value as CEV

import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

fetchCompletions :: IndexStoreEntry -> J.Position -> IO [J.CompletionItem]
fetchCompletions entry pos = do
    -- TODO: Context-awareness (through nested envs?)
    -- TODO: Filter based on user input (may require using the VFS to track updated document in real-time)
    let env = maybeToList $ compilerEnv entry
        completions = toCompletionItem <$> ((CT.allBindings . CE.valueEnv) =<< env)
    logs INFO $ "fetchCompletions: Found " ++ show (length completions) ++ " completions"
    return completions

toCompletionItem :: (CI.QualIdent, CEV.ValueInfo) -> J.CompletionItem
toCompletionItem (qident, _) = J.CompletionItem name Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    where name = T.pack $ CI.idName $ CI.qidIdent qident
