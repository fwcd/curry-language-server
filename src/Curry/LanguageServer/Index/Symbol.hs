module Curry.LanguageServer.Index.Symbol (
    SymbolKind (..),
    Symbol (..)
) where

import qualified Data.Text as T
import qualified Language.LSP.Types as J

-- | The 'kind' of the symbol in the LSP sense.
data SymbolKind = Value | Module | TypeData | TypeNew | TypeClass | TypeAlias | TypeVar

-- | A type or value. If it's a type, the 'printed type' will be the printed kind.
data Symbol = Symbol
    { sKind :: SymbolKind
    , sQualIdent :: T.Text
    , sPrintedType :: T.Text
    , sArrowArity :: Int
    , sLocation :: J.Location
    }
