{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Index.Symbol (
    SymbolKind (..),
    Symbol (..),
    sParentIdent
) where

import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Types as J

-- | The 'kind' of the symbol in the LSP sense.
data SymbolKind = ValueFunction
                | ValueConstructor
                | Module
                | TypeData
                | TypeNew
                | TypeClass
                | TypeAlias
                | TypeVar
                | Other

-- | A type or value. If it's a type, the 'printed type' will be the printed kind.
data Symbol = Symbol
    { sKind :: SymbolKind
    , sQualIdent :: T.Text
    , sIdent :: T.Text
    , sPrintedType :: Maybe T.Text
    , sArrowArity :: Maybe Int
    , sConstructors :: [T.Text]
    , sLocation :: Maybe J.Location
    }

instance Default Symbol where
    def = Symbol
        { sKind = Other
        , sQualIdent = ""
        , sIdent = ""
        , sPrintedType = Nothing
        , sArrowArity = Nothing
        , sConstructors = []
        , sLocation = Nothing
        }

sParentIdent :: Symbol -> T.Text
sParentIdent s = fromMaybe "" $ T.stripSuffix ("." <> sIdent s) $ sQualIdent s