module DinoRush
  ( main
  ) where

import qualified SDL
import qualified SDL.Image as Image
import qualified Animate
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT, asks)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, gets, modify)
import Data.StateVar (($=))
import SDL.Vect

import DinoRush.Clock
import DinoRush.Input
import DinoRush.Logger
import DinoRush.Renderer
import DinoRush.SDL.Input
import DinoRush.SDL.Renderer
import DinoRush.Scene
import DinoRush.Scene.Main
import DinoRush.Scene.Title
import DinoRush.Scene.Pause
import DinoRush.Scene.Play
import DinoRush.Sprite
import DinoRush.Types

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface <- Image.load path
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720, SDL.windowMode = SDL.Fullscreen }
  SDL.showWindow window
  screen <- SDL.getWindowSurface window
  backgroundFar <- loadSurface "data/background_far.png" Nothing
  backgroundNear <- loadSurface "data/background_near.png" Nothing
  foreground <- loadSurface "data/foreground.png" Nothing
  nearground <- loadSurface "data/nearground.png" Nothing
  spriteSheet <- Animate.readSpriteSheetJSON loadSurface "data/dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Surface Seconds)
  runDinoRush (Config window screen backgroundFar backgroundNear foreground nearground spriteSheet) initVars mainLoop
  SDL.destroyWindow window
  SDL.quit

newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config v (DinoRush m) = evalStateT (runReaderT m config) v

instance Clock DinoRush where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger DinoRush where
  logText = liftIO . T.putStrLn

instance SDLRenderer DinoRush where
  updateWindowSurface = updateWindowSurface'
  clearSurface = clearSurface'
  drawSurfaceToSurface = drawSurfaceToSurface'

instance SDLInput DinoRush where
  pollEventPayloads = pollEventPayloads'

instance HasInput DinoRush where
  updateInput = updateInput'
  getInput = gets vInput
  setInput s = modify (\v -> v { vInput = s })

instance SceneManager DinoRush where
  toScene = toScene'

instance Renderer DinoRush where
  clearScreen = clearScreen'
  drawScreen = drawScreen'

instance Title DinoRush where
  titleStep = titleStep'

instance Play DinoRush where
  playStep = playStep'

instance Pause DinoRush where
  pauseStep = pauseStep'

instance SpriteManager DinoRush where
  getDinoAnimations = getSpriteAnimations cDinoSpriteSheet
  drawDino = drawSprite cDinoSpriteSheet
  drawBackgroundFar = drawHorizontalScrollImage cBackgroundFar
  drawBackgroundNear = drawHorizontalScrollImage cBackgroundNear
  drawForeground = drawHorizontalScrollImage cForeground
  drawNearground = drawHorizontalScrollImage cNearground
