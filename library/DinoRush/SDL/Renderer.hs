module DinoRush.SDL.Renderer where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import SDL.Vect

class Monad m => SDLRenderer m where
  updateWindowSurface :: SDL.Window -> m ()
  clearSurface :: SDL.Surface -> m ()
  drawSurfaceToSurface :: SDL.Surface -> SDL.Surface -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Point V2 CInt) -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window

clearSurface' :: MonadIO m => SDL.Surface -> m ()
clearSurface' screen = liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0)

drawSurfaceToSurface' :: MonadIO m => SDL.Surface -> SDL.Surface -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Point V2 CInt) -> m ()
drawSurfaceToSurface' screen surface maybeClip maybeLoc = do
  _ <- liftIO $ SDL.surfaceBlit surface maybeClip screen maybeLoc
  return ()
