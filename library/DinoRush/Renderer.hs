module DinoRush.Renderer where

import qualified SDL

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import SDL.Vect

class Monad m => Renderer m where
  updateWindowSurface :: m ()
  clearScreen :: m ()
  drawSurfaceToScreen :: SDL.Surface -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Point V2 CInt) -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window

clearScreen' :: MonadIO m => SDL.Surface -> m ()
clearScreen' screen = liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0)

drawSurfaceToScreen' :: MonadIO m => SDL.Surface -> SDL.Surface -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Point V2 CInt) -> m ()
drawSurfaceToScreen' screen surface maybeClip maybeLoc = do
  _ <- liftIO $ SDL.surfaceBlit surface maybeClip screen maybeLoc
  return ()
