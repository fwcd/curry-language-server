{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Curry.LanguageServer.Config (Config (..)) where

import Data.Aeson
import Data.Default

data Config = Config { forceRecompilation :: Bool,
                       importPaths :: [FilePath],
                       libraryPaths :: [FilePath],
                       logLevel :: String,
                       cpmPath :: String }
    deriving (Show, Eq)

instance Default Config where
    def = Config { forceRecompilation = False, importPaths = [], libraryPaths = [], logLevel = "info", cpmPath = "cypm" }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        c <- o .: "curry"
        l <- c .: "languageServer"
        forceRecompilation <- l .:? "forceRecompilation" .!= forceRecompilation def
        importPaths <- l .:? "importPaths" .!= importPaths def
        libraryPaths <- l .:? "libraryPaths" .!= libraryPaths def
        logLevel <- l .:? "logLevel" .!= logLevel def
        cpmPath <- l .:? "cpmPath" .!= cpmPath def
        return Config {..}

instance ToJSON Config where
    toJSON Config {..} = object ["curry" .= object [ "languageServer" .= object [
            "forceRecompilation" .= forceRecompilation,
            "importPaths" .= importPaths,
            "libraryPaths" .= libraryPaths,
            "logLevel" .= logLevel,
            "cpmPath" .= cpmPath
        ]]]
        
