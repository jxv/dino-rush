module DinoRush
  ( main
  ) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import SDL.Vect
import System.Random

import DinoRush.Config
import DinoRush.Effect.Audio
import DinoRush.Effect.Camera
import DinoRush.Effect.Clock
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Effect.HUD
import DinoRush.Effect.Sfx
import DinoRush.Engine.Obstacle
import DinoRush.Wrapper.SDLInput
import DinoRush.Wrapper.SDLRenderer
import DinoRush.Manager.Input
import DinoRush.Manager.Scene
import DinoRush.Resource
import DinoRush.Runner
import DinoRush.Scene.Title
import DinoRush.Scene.Pause
import DinoRush.Scene.Play
import DinoRush.Scene.Death
import DinoRush.Scene.GameOver
import DinoRush.State

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  mkObstacles <- streamOfObstacles <$> getStdGen
  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        }
  runDinoRush cfg (initVars mkObstacles) mainLoop
  SDL.destroyWindow window
  freeResources resources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit

newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config v (DinoRush m) = evalStateT (runReaderT m config) v

instance Audio DinoRush where
  playGameMusic = playGameMusic'
  stopGameMusic = stopGameMusic'
  playJumpSfx = playJumpSfx'
  playDuckSfx = playDuckSfx'
  playPointSfx = playPointSfx'
  playBirdSfx = playBirdSfx'
  playHurtSfx = playHurtSfx'
  playLavaSfx = playLavaSfx'
  playQuakeSfx = playQuakeSfx'
  playRockSfx = playRockSfx'
  playRecoverSfx = playRecoverSfx'
  playDeathSfx = playDeathSfx'
  lowerGameMusic = lowerGameMusic'
  raiseGameMusic = raiseGameMusic'
  playStockSfx = playStockSfx'

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
  getDinoAnimations = getSpriteAnimations (rDinoSprites . cResources)
  getLavaAnimations = getSpriteAnimations (rLavaSprites . cResources)
  getRockAnimations = getSpriteAnimations (rRockSprites . cResources)
  getBirdAnimations = getSpriteAnimations (rBirdSprites . cResources)
  getMountainAnimations = getSpriteAnimations (rMountainSprites . cResources)
  getRiverAnimations = getSpriteAnimations (rRiverSprites . cResources)
  drawDino = drawSprite (rDinoSprites . cResources)
  drawLava = drawSprite (rLavaSprites . cResources)
  drawRock = drawSprite (rRockSprites . cResources)
  drawBird = drawSprite (rBirdSprites . cResources)
  drawMountain = drawHorizontalScrollSprite (rMountainSprites . cResources) 16
  drawJungle = drawHorizontalScrollImage (rJungleSprites . cResources)
  drawGround = drawHorizontalScrollImage (rGroundSprites . cResources)
  drawRiver = drawHorizontalScrollSprite (rRiverSprites . cResources) 4
  drawBlackOverlay = drawBlackOverlay'
  drawHiscoreText = drawTextureSprite (rHiscoreSprite . cResources)
  drawPauseText = drawTextureSprite (rPauseSprite . cResources)
  drawGameOverText = drawTextureSprite (rGameOverSprite . cResources)
  drawPressSpaceText = drawTextureSprite (rSpaceSprite . cResources)
  drawPressEscapeText = drawTextureSprite (rEscapeSprite . cResources)
  drawTitleText = drawTextureSprite (rTitleSprite . cResources)
  drawNumber n = drawTextureSprite (flip rNumberSprites n . cResources)

instance Title DinoRush where
  titleStep = titleStep'

instance Play DinoRush where
  playStep = playStep'

instance Pause DinoRush where
  pauseStep = pauseStep'

instance Death DinoRush where
  deathStep = deathStep'

instance GameOver DinoRush where
  gameOverStep = gameOverStep'

instance CameraControl DinoRush where
  adjustCamera = adjustCamera'
  disableZoom = disableZoom'
  enableZoom = enableZoom'

instance HUD DinoRush where
  drawScore = drawScore'
  drawHiscore = drawHiscore'

instance AudioSfx DinoRush where
  playSfx = playSfx'
  clearSfx = clearSfx'
  addSfxs = addSfxs'
