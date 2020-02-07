{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Hover (fetchHover) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS
import qualified Base.TopEnv as CT
import qualified CompilerEnv as CE
import qualified Env.Value as CEV

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.IndexStore (IndexStoreEntry (..))
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchHover :: IndexStoreEntry -> J.Position -> IO (Maybe J.Hover)
fetchHover entry pos = runMaybeT $ do
    ast@(CS.Module _ _ mident _ _ _) <- liftMaybe $ moduleAST entry -- TODO: Has a LayoutInfo argument in newer curry-base versions
    expr <- liftMaybe $ elementAt pos $ expressions ast
    env <- liftMaybe $ CE.valueEnv <$> compilerEnv entry
    hover <- liftMaybe $ toHover mident env expr
    liftIO $ logs INFO $ "fetchHover: Found " ++ show hover
    return hover

toHover :: Show a => CI.ModuleIdent -> CEV.ValueEnv -> CS.Expression a -> Maybe J.Hover
toHover mident env e = (flip J.Hover range) <$> msg
    where range = currySpanInfo2Range $ CSPI.getSpanInfo e
          ident = case e of
              CS.Variable _ _ ident -> Just ident
              CS.Constructor _ _ ident -> Just ident
              _ -> Nothing
          valueInfo = listToMaybe =<< flip CEV.qualLookupValue env <$> ident
          msg = J.HoverContents <$> J.markedUpContent "curry" <$> ppToText <$> CT.origName <$> valueInfo
