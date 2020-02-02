{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Config where

import Data.Aeson
import Data.Default
import System.FilePath

data Config = Config { importPaths :: [FilePath] }

instance Default Config where
    def = Config { importPaths = [] }

instance FromJSON Config where
    parseJSON = withObject "Config.curry.languageServer" $ \c -> Config
        <$> c .:? "importPaths" .!= importPaths def

instance ToJSON Config where
    toJSON Config {..} = object ["curry.languageServer" .= object [
            "importPaths" .= importPaths
        ]]
        
