{-# LANGUAGE LambdaCase #-}
module Curry.LanguageServer.Utils.Concurrent (
    debounce
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forever, void)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)

-- | A simple debouncer that fires when n microseconds have passed since the last call.
-- Source: https://www.reddit.com/r/haskell/comments/ky1llf/concurrent_programming_puzzle_debouncing_events/gjmpbqg
debounce :: (MonadIO m, MonadUnliftIO m) => Int -> (a -> m ()) -> m (a -> m ())
debounce delay action = do
    chan <- liftIO newChan
    runInIO <- askRunInIO

    void $ liftIO $ async $ forever $ do
        x0 <- readChan chan

        -- Wait until no more entries arrive for 'delay' microseconds, then fire with the last entry
        flip fix x0 $ \loop x -> do
            race (readChan chan) (threadDelay delay) >>= \case
                Left x'  -> loop x'
                Right () -> runInIO $ action x
    
    return $ liftIO . writeChan chan
