{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Curry.LanguageServer.Config (Config (..)) where

import Data.Aeson
import Data.Default (Default(..))

data Config = Config { cfgForceRecompilation :: Bool
                     , cfgImportPaths :: [FilePath]
                     , cfgLibraryPaths :: [FilePath]
                     , cfgLogLevel :: String
                     , cfgCurryPath :: String
                     , cfgUseSnippetCompletions :: Bool
                     }
    deriving (Show, Eq)

instance Default Config where
    def = Config { cfgForceRecompilation = False
                 , cfgImportPaths = []
                 , cfgLibraryPaths = []
                 , cfgLogLevel = "info"
                 , cfgCurryPath = "pakcs"
                 , cfgUseSnippetCompletions = False
                 }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        c <- o .: "curry"
        l <- c .: "languageServer"
        cfgForceRecompilation    <- l .:? "forceRecompilation"    .!= cfgForceRecompilation def
        cfgImportPaths           <- l .:? "importPaths"           .!= cfgImportPaths def
        cfgLibraryPaths          <- l .:? "libraryPaths"          .!= cfgLibraryPaths def
        cfgLogLevel              <- l .:? "logLevel"              .!= cfgLogLevel def
        cfgCurryPath             <- l .:? "curryPath"             .!= cfgCurryPath def
        cfgUseSnippetCompletions <- l .:? "useSnippetCompletions" .!= cfgUseSnippetCompletions def
        return Config {..}

instance ToJSON Config where
    toJSON Config {..} = object
        ["curry" .= object
            [ "languageServer" .= object
                [ "forceRecompilation"    .= cfgForceRecompilation
                , "importPaths"           .= cfgImportPaths
                , "libraryPaths"          .= cfgLibraryPaths
                , "logLevel"              .= cfgLogLevel
                , "curryPath"             .= cfgCurryPath
                , "useSnippetCompletions" .= cfgUseSnippetCompletions
                ]
            ]
        ]
        
