{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Main where

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
import DinoRush.Scene
import DinoRush.Title (titleStep, initTitleVars, TitleVars(..), HasTitleVars(..))
import DinoRush.Types

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vInput :: Input
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars Scene'Title Scene'Title initTitleVars (initPlayVars []) initInput

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Vars where
  playVars = lens vPlay (\v s -> v { vPlay = s })

makeClassy ''Vars

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = modify $ titleVars .~ initTitleVars

toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

sceneLoop :: (MonadReader Config m, MonadState Vars m, SceneManager m, Logger m, Clock m, Renderer m, HasInput m, SDLInput m) => m ()
sceneLoop = do
  events <- pollEventPayloads
  input <- getInput
  setInput (stepControl events input)
  clearScreen
  scene <- gets vScene
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
