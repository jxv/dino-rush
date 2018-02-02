{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene where

import qualified SDL
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))

import DinoRush.GameOver
import DinoRush.Clock
import DinoRush.Play
import DinoRush.Pause
import DinoRush.Logger
import DinoRush.Renderer
import DinoRush.Input
import DinoRush.Title (titleStep, initTitleVars, TitleVars(..), HasTitleVars(..))
import DinoRush.Types

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vEventPayloads :: [SDL.EventPayload]
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars Scene'Title Scene'Title initTitleVars []

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v t -> v { vTitle = t })

makeClassy ''Vars

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = modify $ titleVars .~ initTitleVars

sceneLoop :: (MonadReader Config m, MonadState Vars m, Logger m, Clock m, Renderer m, Input m) => m ()
sceneLoop = do
  scene <- gets vScene
  events <- pollEventPayloads
  setEventPayloads events
  clearScreen
  case scene of
    Scene'Title -> titleStep
    Scene'Play -> playStep
    Scene'Pause -> pauseStep
    Scene'GameOver -> gameOverStep
    Scene'Quit -> return ()
  updateWindowSurface
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  let quit = nextScene == Scene'Quit || elem SDL.QuitEvent events
  modify (\v -> v { vScene = nextScene })
  unless quit sceneLoop
