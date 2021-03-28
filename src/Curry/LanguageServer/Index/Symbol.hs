module Curry.LanguageServer.Index.Symbol (
    SymbolKind (..),
    Symbol (..)
) where

import qualified Data.Text as T

-- | The 'kind' of the symbol in the LSP sense.
data SymbolKind = Value | Module | TypeData | TypeNew | TypeClass | TypeAlias | TypeVar

-- | A type or value. If it's a type, the 'printed type' will be the printed kind.
data Symbol = Symbol
    { sKind :: SymbolKind
    , sPrintedType :: T.Text
    , sArrowArity :: Int
    , sQualIdent :: T.Text
    }
