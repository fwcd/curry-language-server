{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, RecordWildCards, TypeApplications #-}
module Curry.LanguageServer.Extension
    ( ExtensionPoint (..), Extension (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:?), (.!=), (.=), object, withObject)
import Data.Default (Default (..))
import qualified Data.Text as T

data ExtensionPoint = ExtensionPointHover
                    | ExtensionPointUnknown T.Text
    deriving (Show, Eq)

data Extension = Extension
    { name :: T.Text
    , extensionPoint :: ExtensionPoint
    , executable :: T.Text
    , args :: [T.Text]
    }
    deriving (Show, Eq)

instance Default Extension where
    def = Extension
        { name           = "Anonymous Extension"
        , extensionPoint = ExtensionPointHover
        , executable     = "echo"
        , args           = []
        }

instance FromJSON Extension where
    parseJSON = withObject "Extension" $ \e -> do
        name           <- e .:? "name"           .!= (def @Extension).name
        extensionPoint <- e .:? "extensionPoint" .!= (def @Extension).extensionPoint
        executable     <- e .:? "executable"     .!= (def @Extension).executable
        args           <- e .:? "args"           .!= (def @Extension).args
        return Extension {..}

instance ToJSON Extension where
    toJSON Extension {..} = object
        [ "name"           .= name
        , "extensionPoint" .= extensionPoint
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
