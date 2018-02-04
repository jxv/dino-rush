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
import DinoRush.Scene
import DinoRush.Main
import DinoRush.Types

import DinoRush.Title

--

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface <- Image.load path
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

--

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  SDL.showWindow window
  screen <- SDL.getWindowSurface window
  spriteSheet <- Animate.readSpriteSheetJSON loadSurface "dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Surface Seconds)
  runDinoRush (Config window screen spriteSheet) initVars sceneLoop
  SDL.destroyWindow window
  SDL.quit

--

newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config v (DinoRush m) = evalStateT (runReaderT m config) v

instance Clock DinoRush where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger DinoRush where
  logText = liftIO . T.putStrLn

instance Renderer DinoRush where
  updateWindowSurface = do
    window <- asks cWindow
    updateWindowSurface' window
  clearScreen = do
    screen <- asks cScreen
    clearScreen' screen
  drawSurfaceToScreen surface maybeClip maybeLoc = do
    screen <- asks cScreen
    drawSurfaceToScreen' screen surface maybeClip maybeLoc

instance SDLInput DinoRush where
  pollEventPayloads = pollEventPayloads'

instance HasInput DinoRush where
  getInput = gets vInput
  setInput s = modify (\v -> v { vInput = s })

instance SceneManager DinoRush where
  toScene = toScene'
