{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, RecordWildCards, TypeApplications #-}
module Curry.LanguageServer.Extension
    ( ExtensionPoint (..), ExtensionOutputFormat (..), Extension (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:?), (.!=), (.=), object, withObject)
import Data.Default (Default (..))
import qualified Data.Text as T

data ExtensionPoint = ExtensionPointHover
                    | ExtensionPointUnknown T.Text
    deriving (Show, Eq)

data ExtensionOutputFormat = ExtensionOutputFormatPlaintext
                           | ExtensionOutputFormatMarkdown
                           | ExtensionOutputFormatUnknown T.Text
    deriving (Show, Eq)

data Extension = Extension
    { name           :: T.Text
    , extensionPoint :: ExtensionPoint
    , outputFormat   :: ExtensionOutputFormat
    , executable     :: T.Text
    , args           :: [T.Text]
    }
    deriving (Show, Eq)

instance Default Extension where
    def = Extension
        { name           = "Anonymous Extension"
        , extensionPoint = ExtensionPointHover
        , outputFormat   = ExtensionOutputFormatPlaintext
        , executable     = "echo"
        , args           = []
        }

instance FromJSON Extension where
    parseJSON = withObject "Extension" $ \e -> do
        name           <- e .:? "name"           .!= (def @Extension).name
        extensionPoint <- e .:? "extensionPoint" .!= (def @Extension).extensionPoint
        outputFormat   <- e .:? "outputFormat"   .!= (def @Extension).outputFormat
        executable     <- e .:? "executable"     .!= (def @Extension).executable
        args           <- e .:? "args"           .!= (def @Extension).args
        return Extension {..}

instance ToJSON Extension where
    toJSON Extension {..} = object
        [ "name"           .= name
        , "extensionPoint" .= extensionPoint
        , "outputFormat"   .= outputFormat
        , "executable"     .= executable
        , "args"           .= args
        ]

instance FromJSON ExtensionPoint where
    parseJSON v = do
        s <- parseJSON v
        return $ case s :: T.Text of
            "hover" -> ExtensionPointHover
            _       -> ExtensionPointUnknown s

instance ToJSON ExtensionPoint where
    toJSON p = toJSON @T.Text $ case p of
        ExtensionPointHover     -> "hover"
        ExtensionPointUnknown s -> s

instance FromJSON ExtensionOutputFormat where
    parseJSON v = do
        s <- parseJSON v
        return $ case s :: T.Text of
            "plaintext" -> ExtensionOutputFormatPlaintext
            "markdown"  -> ExtensionOutputFormatMarkdown
            _           -> ExtensionOutputFormatUnknown s

instance ToJSON ExtensionOutputFormat where
    toJSON p = toJSON @T.Text $ case p of
        ExtensionOutputFormatPlaintext -> "plaintext"
        ExtensionOutputFormatMarkdown  -> "markdown"
        ExtensionOutputFormatUnknown s -> s
