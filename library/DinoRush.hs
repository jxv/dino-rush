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
import Control.Concurrent (threadDelay)
import Data.StateVar (($=))
import Foreign.C.Types
import SDL.Vect
import Data.Text (Text)

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.Key DinoKey
instance Animate.KeyName DinoKey where
  keyName = dinoKey'keyName

dinoKey'keyName :: DinoKey -> Text
dinoKey'keyName = \case
  DinoKey'Idle -> "Idle"
  DinoKey'Move -> "Move"
  DinoKey'Kick -> "Kick"
  DinoKey'Hurt -> "Hurt"
  DinoKey'Sneak -> "Sneak"

--

class Monad m => Clock m where
  delayMilliseconds :: Int -> m ()

delayMilliseconds' :: Int -> IO ()
delayMilliseconds' ms = threadDelay (1000 * ms)

--

class Monad m => Logger m where
  logText :: Text -> m ()

--

class Monad m => Input m where
  getEventPayloads :: m [SDL.EventPayload]

--

class Monad m => Renderer m where
  updateWindowSurface :: m ()
  clearScreen :: m ()
  drawSurfaceToScreen :: SDL.Surface -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Point V2 CInt) -> m ()

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
  spriteSheet <- Animate.readSpriteSheetJSON loadSurface "dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Surface Float)
  runDinoRush (Config window screen spriteSheet) (Vars $ Animate.initPosition DinoKey'Idle) loop
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
  pos <- gets vDinoAnimationPosition
  events <- getEventPayloads
  let quit = elem SDL.QuitEvent events
  let toNextKey = any detectSpacePressed events
  let pos' = Animate.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Animate.currentLocation ssAnimations pos'
  clearScreen
  drawSurfaceToScreen ssImage (Just $ rectFromClip loc) Nothing
  updateWindowSurface
  delayMilliseconds frameDeltaMilliseconds
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ logText $ Animate.keyName (Animate.pKey pos'')
  modify (\vars -> vars { vDinoAnimationPosition = pos'' })
  unless quit loop
  where
    frameDeltaSeconds = 0.016667
    frameDeltaMilliseconds = 16

--

data Config = Config
  { cWindow :: SDL.Window
  , cScreen :: SDL.Surface
  , cDinoSpriteSheet :: Animate.SpriteSheet DinoKey SDL.Surface Float
  }

data Vars = Vars
  { vDinoAnimationPosition :: Animate.Position DinoKey Float
  }

newtype DinoRush a = DinoRush (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runDinoRush :: Config -> Vars -> DinoRush a -> IO a
runDinoRush config vars (DinoRush m) = evalStateT (runReaderT m config) vars

instance Clock DinoRush where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger DinoRush where
  logText = liftIO . T.putStrLn

instance Renderer DinoRush where
  updateWindowSurface = do
    window <- asks cWindow
    liftIO $ SDL.updateWindowSurface window
  clearScreen = do
    screen <- asks cScreen
    liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0)
  drawSurfaceToScreen surface maybeClip maybeLoc = do
    screen <- asks cScreen
    _ <- liftIO $ SDL.surfaceBlit surface maybeClip screen maybeLoc
    return ()

instance Input DinoRush where
  getEventPayloads = liftIO $ map SDL.eventPayload <$> SDL.pollEvents
