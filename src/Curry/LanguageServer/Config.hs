{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot, RecordWildCards, OverloadedStrings, TypeApplications #-}
module Curry.LanguageServer.Config
    ( Config (..)
    , LogLevel (..)
    ) where

import Colog.Core (Severity (..))
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , (.!=)
    , (.:?)
    , withObject
    , object
    , KeyValue (..)
    )
import Data.Default (Default(..))
import qualified Data.Text as T

newtype LogLevel = LogLevel { severity :: Severity }
    deriving (Show, Eq)

data Config = Config { forceRecompilation :: Bool
                     , importPaths :: [FilePath]
                     , libraryPaths :: [FilePath]
                     , logLevel :: LogLevel
                     , curryPath :: String
                     , useSnippetCompletions :: Bool
                     }
    deriving (Show, Eq)

instance Default Config where
    def = Config { forceRecompilation = False
                 , importPaths = []
                 , libraryPaths = []
                 , logLevel = LogLevel Info
                 , curryPath = "pakcs"
                 , useSnippetCompletions = False
                 }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \l -> do
        forceRecompilation    <- l .:? "forceRecompilation"    .!= (def @Config).forceRecompilation
        importPaths           <- l .:? "importPaths"           .!= (def @Config).importPaths
        libraryPaths          <- l .:? "libraryPaths"          .!= (def @Config).libraryPaths
        logLevel              <- l .:? "logLevel"              .!= (def @Config).logLevel
        curryPath             <- l .:? "curryPath"             .!= (def @Config).curryPath
        useSnippetCompletions <- l .:? "useSnippetCompletions" .!= (def @Config).useSnippetCompletions
        return Config {..}

instance ToJSON Config where
    toJSON Config {..} = object
        [ "forceRecompilation"    .= forceRecompilation
        , "importPaths"           .= importPaths
        , "libraryPaths"          .= libraryPaths
        , "logLevel"              .= logLevel
        , "curryPath"             .= curryPath
        , "useSnippetCompletions" .= useSnippetCompletions
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
