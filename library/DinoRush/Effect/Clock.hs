module DinoRush.Effect.Clock where

import Control.Concurrent (threadDelay)

class Monad m => Clock m where
  delayMilliseconds :: Int -> m ()

delayMilliseconds' :: Int -> IO ()
delayMilliseconds' ms = threadDelay (1000 * ms)
