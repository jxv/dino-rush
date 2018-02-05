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
  drawBackgroundFar :: Int -> m ()

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
