module DinoRush.Renderer where

import qualified Animate
import qualified SDL
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader

import DinoRush.SDL.Renderer
import DinoRush.Types

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  screen <- asks cScreen
  clearSurface screen

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  window <- asks cWindow
  updateWindowSurface window
