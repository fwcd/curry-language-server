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
    parseJSON = withObject "Config.curry.languageServer" $ \c -> Config
        <$> c .:? "importPaths" .!= importPaths def
        <*> c .:? "libraryPaths" .!= libraryPaths def

instance ToJSON Config where
    toJSON Config {..} = object ["curry.languageServer" .= object [
            "importPaths" .= importPaths,
            "libraryPaths" .= libraryPaths
        ]]
        
