{-# LANGUAGE LambdaCase #-}
module Curry.LanguageServer.Utils.Concurrent (
    Debouncer,
    ConstDebouncer,
    debounce,
    debounceConst
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race, cancel)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Curry.LanguageServer.Utils.General ((<.$>))

type Debouncer a n = (a -> n (), n ())
type ConstDebouncer n = (n (), n ())

-- | A simple debouncer that fires when n microseconds have passed since the last call.
-- Source: https://www.reddit.com/r/haskell/comments/ky1llf/concurrent_programming_puzzle_debouncing_events/gjmpbqg
debounce :: (MonadIO m, MonadUnliftIO m, MonadIO n) => Int -> (a -> m ()) -> m (Debouncer a n)
debounce delay action = do
    chan <- liftIO newChan
    runInIO <- askRunInIO

    worker <- liftIO $ async $ forever $ do
        x0 <- readChan chan

        -- Wait until no more entries arrive for 'delay' microseconds, then fire with the last entry
        flip fix x0 $ \loop x -> do
            race (readChan chan) (threadDelay delay) >>= \case
                Left x'  -> loop x'
                Right () -> runInIO $ action x
    
    let debounced = liftIO . writeChan chan
        canceller = liftIO $ cancel worker
    return (debounced, canceller)

-- | Debounces an action without parameters.
debounceConst :: (MonadIO m, MonadUnliftIO m, MonadIO n) => Int -> m () -> m (ConstDebouncer n)
debounceConst delay action = ($ ()) <.$> debounce delay (const action)
