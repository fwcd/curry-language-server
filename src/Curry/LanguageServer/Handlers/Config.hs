{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Handlers.Config
    ( onConfigChange
    ) where

import Curry.LanguageServer.Config (Config (..))
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Logging (infoM)

onConfigChange :: Config -> LSM ()
onConfigChange cfg = do
    infoM "Changed configuration"
