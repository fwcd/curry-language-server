module Curry.LanguageServer.Utils.General (
    lastSafe,
    rangeElem,
    pointRange,
    emptyRange,
    maybeCons,
    walkFiles,
    liftMaybe,
    slipr3, slipr4,
    (<.$>), (<$.>),
    joinFst, joinSnd
) where

import Control.Monad (join)
import Control.Monad.Trans.Maybe
import qualified Language.Haskell.LSP.Types as J
import System.FilePath
import System.Directory

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

-- | Lists files in the directory recursively.
walkFiles :: FilePath -> IO [FilePath]
walkFiles fp = do
    isFile <- doesFileExist fp
    if isFile
        then return [fp]
        else do
            isDirectory <- doesDirectoryExist fp
            if isDirectory
                then do
                    contents <- ((fp </>) <$>) <$> listDirectory fp
                    join <$> (sequence $ walkFiles <$> contents)
                else return []

-- | Lifts a Maybe into a Maybe transformer.
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | Moves the first parameter to the end.
slipr3 :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr3 f y z x = f x y z

-- | Moves the first parameter to the end.
slipr4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
slipr4 f y z w x = f x y z w

-- | Maps over the first element of a tuple.
(<.$>) :: Functor f => (a -> c) -> f (a, b) -> f (c, b)
(<.$>) f = fmap $ \(x, y) -> (f x, y)

-- | Maps over the second element of a tuple.
(<$.>) :: Functor f => (b -> c) -> f (a, b) -> f (a, c)
(<$.>) f = fmap $ \(x, y) -> (x, f y)

joinFst :: Monad m => m (m a, b) -> m (a, b)
joinFst m = do
    (mx, y) <- m
    x <- mx
    return (x, y)

joinSnd :: Monad m => m (a, m b) -> m (a, b)
joinSnd m = do
    (x, my) <- m
    y <- my
    return (x, y)
