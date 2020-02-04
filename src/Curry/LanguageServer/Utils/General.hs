module Curry.LanguageServer.Utils.General where

import qualified Language.Haskell.LSP.Types as J

-- | Tests whether a position is inside a given range.
rangeElem :: J.Position -> J.Range -> Bool
rangeElem (J.Position l c) range | l1 == l2  = c1 <= c && c <= c2
                                 | l == l1   = c1 <= c
                                 | l == l2   = c <= c2
                                 | otherwise = l1 <= l && l <= l2
    where (J.Range (J.Position l1 c1) (J.Position l2 c2)) = range
