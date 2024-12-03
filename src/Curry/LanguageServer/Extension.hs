{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications #-}
module Curry.LanguageServer.Extension
    ( ExtensionPoint (..), Extension (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import GHC.Generics (Generic (..))

data ExtensionPoint = ExtensionPointHover
                    | ExtensionPointUnknown T.Text
    deriving (Show, Eq)

data Extension = Extension
    { name :: T.Text
    , extensionPoint :: ExtensionPoint
    , executable :: T.Text
    , args :: [T.Text]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Extension where

instance ToJSON Extension where

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
