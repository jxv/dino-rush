module DinoRush.Input where

import Control.Monad.IO.Class (MonadIO(..))
import qualified SDL

class Monad m => Input m where
  pollEventPayloads :: m [SDL.EventPayload]
  getEventPayloads :: m [SDL.EventPayload]
  setEventPayloads :: [SDL.EventPayload] -> m ()

pollEventPayloads' :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents
