module DinoRush.Input where

import Control.Monad.IO.Class (MonadIO(..))
import qualified SDL

class Monad m => Input m where
  getEventPayloads :: m [SDL.EventPayload]

getEventPayloads' :: MonadIO m => m [SDL.EventPayload]
getEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents
