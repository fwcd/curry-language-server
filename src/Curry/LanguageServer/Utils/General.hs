{-# LANGUAGE OverloadedStrings #-}
-- | General utilities.
module Curry.LanguageServer.Utils.General (
    lastSafe,
    rangeElem,
    nth,
    wordAtIndex, wordAtPos,
    wordsWithSpaceCount,
    pointRange, emptyRange,
    maybeCons,
    walkFiles,
    liftMaybe,
    slipr3, slipr4,
    (<.$>), (<$.>),
    joinFst, joinSnd,
    removeSingle,
    expectJust,
    insertAll,
    groupIntoMapBy,
    groupIntoMapByM,
    fst3, snd3, thd3,
    tripleToPair
) where

import Control.Monad (join)
import Control.Monad.Trans.Maybe
import Data.Foldable (foldrM)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
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

-- | Safely fetches the nth entry.
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth n (x:xs) | n == 0 = Just x
             | n < 0 = Nothing
             | otherwise = nth (n - 1) xs

-- | Finds the word at the given offset.
wordAtIndex :: Int -> T.Text -> Maybe T.Text
wordAtIndex n = wordAtIndex' n . wordsWithSpaceCount
    where wordAtIndex' :: Int -> [(Int, T.Text)] -> Maybe T.Text
          wordAtIndex' _ [] = Nothing
          wordAtIndex' n' ((k, s):ss) | (n' - k) <= len = Just s
                                      | otherwise = wordAtIndex' (n' - len - k) ss
            where len = T.length s

-- | Fetches the words with the list of spaces preceding them.
wordsWithSpaceCount :: T.Text -> [(Int, T.Text)]
wordsWithSpaceCount t | T.null t = []
                      | otherwise = (T.length s, w) : wordsWithSpaceCount t''
                          -- TODO: Implement using T.breakOnAll
                          where s   = T.takeWhile isSpace t
                                t'  = T.dropWhile isSpace t
                                w   = T.takeWhile (not . isSpace) t'
                                t'' = T.dropWhile (not . isSpace) t'

-- | Finds the word at a given position.
wordAtPos :: J.Position -> T.Text -> Maybe T.Text
wordAtPos (J.Position l c) = (T.strip <$>) . (wordAtIndex c =<<) . nth l . T.lines 

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

-- | Removes a single element from the list (returning all possible solutions).
removeSingle :: [a] -> [([a], a)]
removeSingle [] = []
removeSingle (x:xs) = (xs, x) : (x:) <.$> (removeSingle xs)

-- | Force-unwraps a maybe, possible outputting an error message.
expectJust :: String -> Maybe a -> a
expectJust msg Nothing = error msg
expectJust _ (Just x) = x

-- | Inserts all key-value-pairs into the given map.
insertAll :: Ord k => [(k, v)] -> M.Map k v -> M.Map k v
insertAll [] = id
insertAll ((k, v):kvs) = insertAll kvs . M.insert k v

-- | Groups by key into a map.
groupIntoMapBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupIntoMapBy f = foldr (\x -> M.insertWith (++) (f x) [x]) M.empty

-- | Groups by key into a map monadically.
groupIntoMapByM :: (Ord k, Monad m) => (a -> m k) -> [a] -> m (M.Map k [a])
groupIntoMapByM f = foldrM (\x m -> (\y -> M.insertWith (++) y [x] m) <$> f x) M.empty

fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

snd3 :: (a, b, c) -> b
snd3 (x, y, z) = y

thd3 :: (a, b, c) -> c
thd3 (x, y, z) = z

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (x, y, z) = (x, y)
