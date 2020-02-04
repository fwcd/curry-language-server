{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Features.Hover (fetchHover) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Syntax as CS

import Curry.LanguageServer.Compiler
import Curry.LanguageServer.Utils.Conversions
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

fetchHover :: Show a => CompilationResult a -> J.Position -> IO (Maybe J.Hover)
fetchHover compilation pos = do
    let hover = toHover =<< expressionAt pos =<< (moduleAST <$> compilationToMaybe compilation)
    U.logs $ "fetchHover: Found " ++ show hover
    return hover

toHover :: Show a => CS.Expression a -> Maybe J.Hover
toHover e = (flip J.Hover range) <$> msg
    where range = currySpanInfo2Range $ CSPI.getSpanInfo e
          msg = J.HoverContents <$> J.markedUpContent "curry" <$> T.pack <$> (listToMaybe $ words $ show e)
