module Curry.LanguageServer.Utils.General (
    lastSafe,
    rangeElem,
    pointRange,
    emptyRange,
    maybeCons
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

-- | The point range at the origin.
emptyRange :: J.Range
emptyRange = J.Range (J.Position 0 0) (J.Position 0 0)

-- | A range that starts and ends at the given position.
pointRange :: J.Position -> J.Range
pointRange p = J.Range p p

-- | Appends an element at the front if the optional value is present.
maybeCons :: Maybe a -> [a] -> [a]
maybeCons Nothing = id
maybeCons (Just x) = (x:)
