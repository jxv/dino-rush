{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Main where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Renderer
import DinoRush.Input
import DinoRush.Scene
import DinoRush.Scene.GameOver
import DinoRush.Scene.Play
import DinoRush.Scene.Pause
import DinoRush.Scene.Title
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

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })

mainLoop :: (MonadReader Config m, MonadState Vars m, Logger m, Clock m, Renderer m, HasInput m, Title m, Play m, Pause m) => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  scene <- gets vScene
  case scene of
    Scene'Title -> titleStep
    Scene'Play -> playStep
    Scene'Pause -> pauseStep
    Scene'GameOver -> gameOverStep
    Scene'Quit -> return ()
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  let quit = nextScene == Scene'Quit || iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  modify (\v -> v { vScene = nextScene })
  unless quit mainLoop
