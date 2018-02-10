module DinoRush.Wrapper.SDLRenderer where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import SDL.Vect

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  clearRenderer :: SDL.Renderer -> m ()
  queryTexture :: SDL.Texture -> m SDL.TextureInfo
  drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Rectangle CInt) -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window

presentRenderer' :: MonadIO m => SDL.Renderer -> m ()
presentRenderer' = SDL.present

clearSurface' :: MonadIO m => SDL.Surface -> m ()
clearSurface' screen = liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0)

drawTexture' :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
drawTexture' renderer tex maybeClip maybeLoc = SDL.copy renderer tex maybeClip maybeLoc

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' = SDL.clear

queryTexture' ::  MonadIO m => SDL.Texture -> m SDL.TextureInfo
queryTexture' = SDL.queryTexture
