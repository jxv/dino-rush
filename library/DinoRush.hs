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

import DinoRush.Config
import DinoRush.Engine.Types
import DinoRush.Effect.Audio
import DinoRush.Effect.Clock
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Entity.Dino
import DinoRush.Entity.Bird
import DinoRush.Entity.Bouncer
import DinoRush.Entity.Lava
import DinoRush.Entity.Mountain
import DinoRush.Entity.Rock
import DinoRush.Entity.Obstacle
import DinoRush.Wrapper.SDLInput
import DinoRush.Wrapper.SDLRenderer
import DinoRush.Manager.Input
import DinoRush.Manager.Scene
import DinoRush.Runner
import DinoRush.Scene.Title
import DinoRush.Scene.Pause
import DinoRush.Scene.Play
import DinoRush.State

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
  Mixer.openAudio Mixer.defaultAudio 256

  gameMusic <- Mixer.load "resource/v42.mod"
  jumpSfx <- Mixer.load "resource/dino_jump.wav"
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720, SDL.windowMode = SDL.Fullscreen }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  mountainSprites <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/mountain.json" :: IO (Animate.SpriteSheet MountainKey SDL.Texture Seconds)
  backgroundNear <- SDL.createTextureFromSurface renderer =<< loadSurface "resource/jungle.png" Nothing
  foreground <- SDL.createTextureFromSurface renderer =<< loadSurface "resource/ground.png" Nothing
  nearground <- SDL.createTextureFromSurface renderer =<< loadSurface "resource/river.png" Nothing
  spriteSheet <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Texture Seconds)
  birdSprites <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/bird.json" :: IO (Animate.SpriteSheet BirdKey SDL.Texture Seconds)
  bouncerSprites <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/bouncer.json" :: IO (Animate.SpriteSheet BouncerKey SDL.Texture Seconds)
  lavaSprites <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/lava.json" :: IO (Animate.SpriteSheet LavaKey SDL.Texture Seconds)
  rockSprites <- Animate.readSpriteSheetJSON (\path c -> SDL.createTextureFromSurface renderer =<< loadSurface path c) "resource/rock.json" :: IO (Animate.SpriteSheet RockKey SDL.Texture Seconds)
  mkObstacles <- streamOfObstacles <$> getStdGen

  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cMountainSprites = mountainSprites
        , cBackgroundNear = backgroundNear
        , cForeground = foreground
        , cNearground = nearground
        , cDinoSpriteSheet = spriteSheet
        , cBirdSpriteSheet = birdSprites
        , cBouncerSpriteSheet = bouncerSprites
        , cLavaSpriteSheet = lavaSprites
        , cRockSpriteSheet = rockSprites
        , cJumpSfx = jumpSfx
        , cGameMusic = gameMusic
        }

  runDinoRush cfg (initVars mkObstacles) mainLoop

  SDL.destroyWindow window
  SDL.destroyTexture $ Animate.ssImage mountainSprites
  SDL.destroyTexture backgroundNear
  SDL.destroyTexture foreground
  SDL.destroyTexture nearground
  SDL.destroyTexture $ Animate.ssImage spriteSheet
  SDL.destroyTexture $ Animate.ssImage birdSprites
  SDL.destroyTexture $ Animate.ssImage lavaSprites
  SDL.destroyTexture $ Animate.ssImage rockSprites

  Mixer.free gameMusic
  Mixer.free jumpSfx
  Mixer.closeAudio
  Mixer.quit
  SDL.quit

newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config v (DinoRush m) = evalStateT (runReaderT m config) v

instance Audio DinoRush where
  playGameMusic = playGameMusic'
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
  getMountainAnimations = getSpriteAnimations cMountainSprites
  drawDino = drawSprite cDinoSpriteSheet
  drawMountain = drawHorizontalScrollSprite cMountainSprites 16
  drawBackgroundNear = drawHorizontalScrollImage cBackgroundNear
  drawForeground = drawHorizontalScrollImage cForeground
  drawNearground = drawHorizontalScrollImage cNearground

instance Title DinoRush where
  titleStep = titleStep'

instance Play DinoRush where
  playStep = playStep'

instance Pause DinoRush where
  pauseStep = pauseStep'
