{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiWayIf, NoFieldSelectors, OverloadedRecordDot #-}
-- | General utilities.
module Curry.LanguageServer.Utils.General
    ( lastSafe
    , rangeElem, rangeOverlaps
    , nth
    , pair
    , dup
    , wordAtIndex, wordAtPos
    , wordsWithSpaceCount
    , pointRange, emptyRange
    , maybeCons
    , WalkConfiguration (..)
    , walkFiles
    , walkFilesIgnoring
    , walkFilesWith
    , liftMaybe
    , slipr3, slipr4
    , (<.$>), (<$.>)
    , joinFst, joinSnd
    , removeSingle
    , nothingIfNull
    , replaceString
    , snapToLastTokenStart
    , snapToLastTokenEnd
    , Insertable (..)
    , ConstMap (..)
    , insertIntoTrieWith
    , insertAllIntoTrieWith
    , groupIntoMapBy
    , groupIntoMapByM
    , fst3, snd3, thd3
    , tripleToPair
    , filterF
    , dropLast
    ) where

import Control.Monad (join, filterM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString as B
import Data.Bifunctor (first, second)
import Data.Char (isSpace)
import Data.Default (Default (..))
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Foldable (foldrM)
import qualified Data.Text as T
import qualified Data.Trie as TR
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.LSP.Protocol.Types as J
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory

-- | Safely fetches the last element of the given list.
lastSafe :: [a] -> Maybe a
lastSafe xs | null xs = Nothing
            | otherwise = Just $ last xs

-- | Tests whether a position is inside a given range.
rangeElem :: J.Position -> J.Range -> Bool
rangeElem (J.Position l c) range | l1 == l2 && l == l1 = c1 <= c && c <= c2
                                 | l == l1             = c1 <= c
                                 | l == l2             = c <= c2
                                 | otherwise           = l1 <= l && l <= l2
    where (J.Range (J.Position l1 c1) (J.Position l2 c2)) = range

-- | Tests whether two given ranges overlap.
rangeOverlaps :: J.Range -> J.Range -> Bool
rangeOverlaps r1@(J.Range p1 p2) r2@(J.Range p3 p4) = rangeElem p1 r2
                                                   || rangeElem p2 r2
                                                   || rangeElem p3 r1
                                                   || rangeElem p4 r1

-- | Safely fetches the nth entry.
nth :: Integral n => n -> [a] -> Maybe a
nth _ [] = Nothing
nth n (x:xs) | n' == 0 = Just x
             | n' <  0 = Nothing
             | otherwise = nth (n' - 1) xs
    where n' = (fromIntegral n :: Int)

-- | Creates a pair. Useful in conjunction with partial application.
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- | Duplicates.
dup :: a -> (a, a)
dup x = (x, x)

-- | Finds the word at the given offset.
wordAtIndex :: Integral n => n -> T.Text -> Maybe T.Text
wordAtIndex n = wordAtIndex' n . wordsWithSpaceCount
    where wordAtIndex' :: Integral n => n -> [(n, T.Text)] -> Maybe T.Text
          wordAtIndex' _ [] = Nothing
          wordAtIndex' n' ((k, s):ss) | (n' - k) <= len = Just s
                                      | otherwise = wordAtIndex' (n' - len - k) ss
            where len = fromIntegral $ T.length s

-- | Fetches the words with the list of spaces preceding them.
wordsWithSpaceCount :: Integral n => T.Text -> [(n, T.Text)]
wordsWithSpaceCount t | T.null t = []
                      | otherwise = (fromIntegral $ T.length s, w) : wordsWithSpaceCount t''
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

-- | A filtering configuration for a file walk.
data WalkConfiguration m a = WalkConfiguration
    { -- | Executed when entering a new directory. Fetches some directory-specific
      --   state for later use during filtering, e.g. an ignore file. Returning
      --   Nothing causes the walker to skip the the directory.
      onEnter            :: FilePath -> m (Maybe a)
      -- | Tests whether a file or directory should be ignored using the state of
      --   the directory containing the path.
    , shouldIgnore       :: a -> FilePath -> m Bool
      -- | Whether the walk should include directories.
    , includeDirectories :: Bool
      -- | Whether the walk should include files.
    , includeFiles       :: Bool
    }

instance (Default a, Monad m) => Default (WalkConfiguration m a) where
    def = WalkConfiguration
        { onEnter            = const $ return $ Just def
        , shouldIgnore       = const $ const $ return False
        , includeDirectories = False
        , includeFiles       = True
        }

-- | An empty walk configuration.
emptyWalkConfiguration :: Monad m => WalkConfiguration m ()
emptyWalkConfiguration = def

-- | Lists files in the directory recursively.
walkFiles :: MonadIO m => FilePath -> m [FilePath]
walkFiles = walkFilesWith emptyWalkConfiguration

-- | Lists files in the directory recursively, ignoring files matching the given predicate.
walkFilesIgnoring :: MonadIO m => (FilePath -> Bool) -> FilePath -> m [FilePath]
walkFilesIgnoring ignore = walkFilesWith emptyWalkConfiguration
    { shouldIgnore = const $ return . ignore
    }

-- | Lists files in the directory recursively with the given configuration.
walkFilesWith :: MonadIO m => WalkConfiguration m a -> FilePath -> m [FilePath]
walkFilesWith wc fp = (fromMaybe [] <$>) $ runMaybeT $ do
    isDirectory <- liftIO $ unsafeInterleaveIO $ doesDirectoryExist fp
    isFile      <- liftIO $ unsafeInterleaveIO $ doesFileExist fp
    if | isDirectory -> do
            state     <- MaybeT $ wc.onEnter fp
            contents  <- liftIO $ listDirectory fp
            contents' <- map (fp </>) <$> filterM ((not <$>) . lift . wc.shouldIgnore state) contents
            ([fp | wc.includeDirectories] ++) . join <$> mapM (lift . walkFilesWith wc) contents'
       | isFile      -> return [fp | wc.includeFiles]
       | otherwise   -> liftMaybe Nothing

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
(<.$>) f = fmap $ first f

-- | Maps over the second element of a tuple.
(<$.>) :: Functor f => (b -> c) -> f (a, b) -> f (a, c)
(<$.>) f = fmap $ second f

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
removeSingle (x:xs) = (xs, x) : (x:) <.$> removeSingle xs

-- | Wraps a list into a Maybe that is Nothing if the list is empty.
nothingIfNull :: [a] -> Maybe [a]
nothingIfNull [] = Nothing
nothingIfNull xs = Just xs

replaceString :: String -> String -> String -> String
replaceString n r = T.unpack . T.replace (T.pack n) (T.pack r) . T.pack

-- | Moves the cursor back until the beginning of the last token.
snapToLastTokenStart :: Integral n => String -> n -> n
snapToLastTokenStart = snapBack $ dropWhile (not . isSpace) . dropWhile isSpace

-- | Moves the cursor back until a non-whitespace character precedes it (i.e. past the end of the last token).
snapToLastTokenEnd :: Integral n => String -> n -> n
snapToLastTokenEnd = snapBack $ dropWhile isSpace

snapBack :: Integral n => ([a] -> [a]) -> [a] -> n -> n
snapBack f s n = fromIntegral $ length $ f $ reverse $ take (fromIntegral n) s

class Insertable m a | m -> a where
    -- | Inserts a single entry.
    insert :: a -> m -> m
    insert x = insertAll [x]

    -- | Inserts multiple entries.
    insertAll :: Foldable t => t a -> m -> m
    insertAll = flip $ foldr insert

instance Insertable (Maybe a) a where
    insert = const . Just

instance Ord a => Insertable [a] a where
    insert = L.insert

instance Ord k => Insertable (M.Map k v) (k, v) where
    insert = uncurry M.insert

instance Ord a => Insertable (S.Set a) a where
    insert = S.insert

instance Insertable (TR.Trie a) (B.ByteString, a) where
    insert = uncurry TR.insert

-- | A map that 'pins' a key to a value once inserted.
newtype ConstMap k v = ConstMap { ctmMap :: M.Map k v }

instance Functor (ConstMap k) where
    fmap f (ConstMap m) = ConstMap $ fmap f m

instance Foldable (ConstMap k) where
    foldr f x (ConstMap m) = foldr f x m

instance Traversable (ConstMap k) where
    traverse f (ConstMap m) = ConstMap <$> traverse f m

instance Ord k => Insertable (ConstMap k v) (k, v) where
    insert (k, v) (ConstMap m) = ConstMap $ M.insertWith (const id) k v m

instance Ord k => Semigroup (ConstMap k v) where
    -- Note how ConstMap uses a 'flipped' (<>). This is analogous to how
    -- the insertion combiner also corresponds to a 'flipped' const.
    ConstMap m <> ConstMap m' = ConstMap $ M.union m' m

instance Ord k => Monoid (ConstMap k v) where
    mempty = ConstMap M.empty

-- | Inserts the given element into the trie using the combination function.
-- The combination function takes the new value on the left and the old one on the right.
insertIntoTrieWith :: (a -> a -> a) -> B.ByteString -> a -> TR.Trie a -> TR.Trie a
insertIntoTrieWith f s x t | TR.member s t = TR.adjust (f x) s t
                           | otherwise     = TR.insert s x t

-- | Inserts the given elements into the trie using the combination function.
-- The combination function takes the new value on the left and the old one on the right.
insertAllIntoTrieWith :: Foldable t => (a -> a -> a) -> t (B.ByteString, a) -> TR.Trie a -> TR.Trie a
insertAllIntoTrieWith f = flip $ foldr (uncurry $ insertIntoTrieWith f)

-- | Groups by key into a map.
groupIntoMapBy :: (Foldable t, Ord k) => (a -> k) -> t a -> M.Map k [a]
groupIntoMapBy f = foldr (\x -> M.insertWith (++) (f x) [x]) M.empty

-- | Groups by key into a map monadically.
groupIntoMapByM :: (Foldable t, Ord k, Monad m) => (a -> m k) -> t a -> m (M.Map k [a])
groupIntoMapByM f = foldrM (\x m -> (\y -> M.insertWith (++) y [x] m) <$> f x) M.empty

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (x, y, _) = (x, y)

-- | Filter over a foldable value. For [a], filterF = filter.
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldr (\x xs -> if f x then x : xs else xs) []

-- | Drops the last n items.
dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse
