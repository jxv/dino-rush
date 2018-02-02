module DinoRush
  ( main
  ) where

import qualified SDL
import qualified SDL.Image as Image
import qualified Animate
import qualified Data.Text.IO as T

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT, asks)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, modify, gets)
import Data.StateVar (($=))
import Foreign.C.Types
import SDL.Vect

import DinoRush.Collision
import DinoRush.Types
import DinoRush.Scene
import DinoRush.Title
import DinoRush.Play
import DinoRush.GameOver

import DinoRush.Clock
import DinoRush.Input
import DinoRush.Logger
import DinoRush.Renderer

--

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface <- Image.load path
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

rectFromClip :: Animate.SpriteClip -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

--

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Dino Rush" SDL.defaultWindow { SDL.windowInitialSize = V2 320 180 }
  SDL.showWindow window
  screen <- SDL.getWindowSurface window
  spriteSheet <- Animate.readSpriteSheetJSON loadSurface "dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Surface Seconds)
  runDinoRush (Config window screen spriteSheet) (Vars Scene'Title (TitleVars $ Animate.initPosition DinoKey'Idle)) loop
  SDL.destroyWindow window
  SDL.quit
--

detectSpacePressed :: SDL.EventPayload -> Bool
detectSpacePressed event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat = repeated} ->
    code == SDL.KeycodeSpace &&
    motion == SDL.Pressed &&
    not repeated
  _ -> False

loop :: (MonadReader Config m, MonadState Vars m, Logger m, Clock m, Renderer m, Input m) => m ()
loop = do
  Animate.SpriteSheet{ssAnimations, ssImage} <- asks cDinoSpriteSheet
  pos <- gets (tvPlayer . vTitle)
  events <- getEventPayloads
  let quit = elem SDL.QuitEvent events
  let toNextKey = any detectSpacePressed events
  let pos' = Animate.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Animate.currentLocation ssAnimations pos'
  clearScreen
  drawSurfaceToScreen ssImage (Just $ rectFromClip loc) (Just $ SDL.P $ V2 80 60)
  updateWindowSurface
  delayMilliseconds frameDeltaMilliseconds
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ logText $ Animate.keyName (Animate.pKey pos'')
  modify (\v -> v { vTitle = TitleVars { tvPlayer = pos'' } })
  unless quit loop
  where
    frameDeltaSeconds = 0.016667
    frameDeltaMilliseconds = 16

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

instance Input DinoRush where
  getEventPayloads = getEventPayloads'
