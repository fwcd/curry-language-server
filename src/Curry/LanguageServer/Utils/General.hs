module Curry.LanguageServer.Utils.General (
    lastSafe,
    rangeElem
) where

import qualified Language.Haskell.LSP.Types as J

-- | Safely fetches the last element of the given list.
lastSafe :: [a] -> Maybe a
lastSafe xs | null xs = Nothing
            | otherwise = Just $ last xs

-- | Tests whether a position is inside a given range.
rangeElem :: J.Position -> J.Range -> Bool
rangeElem (J.Position l c) range = if l1 == l2 && l == l1 then c1 <= c && c <= c2
                                   else if l == l1 then c1 <= c
                                   else if l == l2 then c <= c2
                                   else l1 <= l && l <= l2
    where (J.Range (J.Position l1 c1) (J.Position l2 c2)) = range
