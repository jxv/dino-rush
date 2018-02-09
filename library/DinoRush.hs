module DinoRush
  ( main
  ) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Image as Image
import qualified Animate
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.StateVar (($=))
import SDL.Vect
import System.Random

import DinoRush.Audio
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
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Mixer.openAudio Mixer.defaultAudio 4096
  mus <- Mixer.load "data/v42.mod"
  jumpSfx <- Mixer.load "data/jump.wav"
  Mixer.playMusic Mixer.Forever mus
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720, SDL.windowMode = SDL.Fullscreen }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  backgroundFar <- SDL.createTextureFromSurface renderer =<< loadSurface "data/background_far.png" Nothing
  backgroundNear <- SDL.createTextureFromSurface renderer =<< loadSurface "data/background_near.png" Nothing
  foreground <- SDL.createTextureFromSurface renderer =<< loadSurface "data/foreground.png" Nothing
  nearground <- SDL.createTextureFromSurface renderer =<< loadSurface "data/nearground.png" Nothing
  spriteSheet <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "data/dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Texture Seconds)
  mkObstacles <- streamOfObstacles <$> getStdGen
  runDinoRush (Config window renderer backgroundFar backgroundNear foreground nearground spriteSheet jumpSfx) (initVars mkObstacles) mainLoop
  SDL.destroyWindow window
  Mixer.free mus
  Mixer.free jumpSfx
  Mixer.closeAudio
  Mixer.quit
  SDL.quit
newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config v (DinoRush m) = evalStateT (runReaderT m config) v

instance Audio DinoRush where
  playJumpSfx = playJumpSfx'

instance Clock DinoRush where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger DinoRush where
  logText = liftIO . T.putStrLn

instance SDLRenderer DinoRush where
  drawTexture = drawTexture'
  presentRenderer = presentRenderer'
  clearRenderer = clearRenderer'
  queryTexture = queryTexture'

instance SDLInput DinoRush where
  pollEventPayloads = pollEventPayloads'

instance HasInput DinoRush where
  updateInput = updateInput'
  getInput = getInput'
  setInput = setInput'

instance SceneManager DinoRush where
  toScene = toScene'

instance Renderer DinoRush where
  clearScreen = clearScreen'
  drawScreen = drawScreen'
  getDinoAnimations = getSpriteAnimations cDinoSpriteSheet
  drawDino = drawSprite cDinoSpriteSheet
  drawBackgroundFar = drawHorizontalScrollImage cBackgroundFar
  drawBackgroundNear = drawHorizontalScrollImage cBackgroundNear
  drawForeground = drawHorizontalScrollImage cForeground
  drawNearground = drawHorizontalScrollImage cNearground

instance Title DinoRush where
  titleStep = titleStep'

instance Play DinoRush where
  playStep = playStep'

instance Pause DinoRush where
  pauseStep = pauseStep'
