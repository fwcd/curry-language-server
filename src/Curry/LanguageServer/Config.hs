{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Curry.LanguageServer.Config (Config (..)) where

import Data.Aeson
import Data.Default
import System.FilePath

data Config = Config { importPaths :: [FilePath], libraryPaths :: [FilePath] }
    deriving (Show, Eq)

instance Default Config where
    def = Config { importPaths = [], libraryPaths = [] }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        c <- o .: "curry"
        l <- c .: "languageServer"
        importPaths <- l .:? "importPaths" .!= def
        libraryPaths <- l .:? "libraryPaths" .!= def
        return Config {..}

instance ToJSON Config where
    toJSON Config {..} = object ["curry" .= object [ "languageServer" .= object [
            "importPaths" .= importPaths,
            "libraryPaths" .= libraryPaths
        ]]]
        
