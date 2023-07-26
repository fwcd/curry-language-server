{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module Curry.LanguageServer.Config
    ( Config (..)
    , LogLevel (..)
    ) where

import Colog.Core (Severity (..))
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , (.!=)
    , (.:)
    , (.:?)
    , withObject
    , object
    , KeyValue (..)
    )
import Data.Default (Default(..))
import qualified Data.Text as T

newtype LogLevel = LogLevel { llSeverity :: Severity }
    deriving (Show, Eq)

data Config = Config { cfgForceRecompilation :: Bool
                     , cfgImportPaths :: [FilePath]
                     , cfgLibraryPaths :: [FilePath]
                     , cfgLogLevel :: LogLevel
                     , cfgCurryPath :: String
                     , cfgUseSnippetCompletions :: Bool
                     }
    deriving (Show, Eq)

instance Default Config where
    def = Config { cfgForceRecompilation = False
                 , cfgImportPaths = []
                 , cfgLibraryPaths = []
                 , cfgLogLevel = LogLevel Info
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
        
instance FromJSON LogLevel where
    parseJSON v = do
        s <- parseJSON v
        return $ case s :: T.Text of
            "debug"   -> LogLevel Debug
            "info"    -> LogLevel Info
            "warning" -> LogLevel Warning
            "error"   -> LogLevel Error
            _         -> undefined

instance ToJSON LogLevel where
    toJSON (LogLevel sev) = toJSON @T.Text $ case sev of
        Debug   -> "debug"
        Info    -> "info"
        Warning -> "warning"
        Error   -> "error"
