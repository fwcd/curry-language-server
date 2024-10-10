{-# LANGUAGE NoFieldSelectors, OverloadedStrings, OverloadedRecordDot #-}
module Curry.LanguageServer.Index.Symbol
    ( SymbolKind (..)
    , Symbol (..)
    , symbolParentIdent
    , symbolIsFromCurrySource
    ) where

import Control.Lens ((^.))
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J

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
    deriving (Show, Eq)

-- | A module, type or value. If it's a type, the 'printed type' will be the printed kind.
data Symbol = Symbol
    { kind :: SymbolKind
    , qualIdent :: T.Text
    , ident :: T.Text
    , printedType :: Maybe T.Text
    , printedArgumentTypes :: [T.Text]
    , printedResultType :: Maybe T.Text
    , arrowArity :: Maybe Int
    , constructors :: [T.Text]
    , location :: Maybe J.Location
    }
    deriving (Show, Eq)

instance Default Symbol where
    def = Symbol
        { kind = Other
        , qualIdent = ""
        , ident = ""
        , printedType = Nothing
        , printedArgumentTypes = []
        , printedResultType = Nothing
        , arrowArity = Nothing
        , constructors = []
        , location = Nothing
        }

symbolParentIdent :: Symbol -> T.Text
symbolParentIdent s = fromMaybe "" $ T.stripSuffix ("." <> s.ident) s.qualIdent

symbolIsFromCurrySource :: Symbol -> Bool
symbolIsFromCurrySource s = maybe False ((".curry" `T.isSuffixOf`) . J.getUri . (^. J.uri)) s.location
