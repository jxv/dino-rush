module DinoRush.Sprite where

import qualified SDL
import qualified Animate

import Control.Monad.Reader (MonadReader, asks)
import DinoRush.Renderer
import DinoRush.Types
import DinoRush.SDL.Renderer

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

class Monad m => SpriteManager m where
  getDinoAnimations :: m (Animations DinoKey)
  drawDino :: DrawSprite DinoKey m
  drawBackgroundFar :: (Int, Int) -> m ()
  drawBackgroundNear :: (Int, Int) -> m ()
  drawForeground :: (Int, Int) -> m ()
  drawNearground :: (Int, Int) -> m ()

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
