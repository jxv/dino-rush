module DinoRush.Renderer where

import qualified Animate
import qualified SDL
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader

import DinoRush.SDL.Renderer
import DinoRush.Types

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  getDinoAnimations :: m (Animations DinoKey)
  drawDino :: DrawSprite DinoKey m
  drawBackgroundFar :: (Int, Int) -> m ()
  drawBackgroundNear :: (Int, Int) -> m ()
  drawForeground :: (Int, Int) -> m ()
  drawNearground :: (Int, Int) -> m ()


clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  screen <- asks cScreen
  clearSurface screen

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  window <- asks cWindow
  updateWindowSurface window

--

backgroundFarY, backgroundNearY, foregroundY, dinoY, neargroundY :: Int
backgroundFarY = -16
backgroundNearY = 16 * 16
foregroundY = 16 * 28
dinoY = 16 * 26
neargroundY = 16 * 36

--

drawSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Surface Seconds) -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawSprite ss clip (x,y) = do
  screen <- asks cScreen
  sheet <- asks (Animate.ssImage . ss)
  drawSurfaceToSurface
    screen
    sheet
    (Just $ rectFromClip clip)
    (Just $ SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y))

getSpriteAnimations :: (MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Surface Seconds) -> m (Animations key)
getSpriteAnimations ss = asks (Animate.ssAnimations . ss)

drawHorizontalScrollImage :: (MonadReader Config m, SDLRenderer m) => (Config -> SDL.Surface) -> (Int, Int) -> m ()
drawHorizontalScrollImage surface (x,y) = do
  screen <- asks cScreen
  background <- asks surface
  drawSurfaceToSurface screen background Nothing (Just $ SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y))
  drawSurfaceToSurface screen background Nothing (Just $ SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y))
