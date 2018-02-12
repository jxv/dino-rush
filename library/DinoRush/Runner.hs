{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import DinoRush.Config
import DinoRush.Effect.Audio
import DinoRush.Effect.Clock
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Play
import DinoRush.Engine.Title
import DinoRush.Manager.Input
import DinoRush.Manager.Scene
import DinoRush.Scene.GameOver
import DinoRush.Scene.Play
import DinoRush.Scene.Pause
import DinoRush.Scene.Title

import DinoRush.State

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = modify $ titleVars .~ initTitleVars

playTransition :: (HasPlayVars a, MonadState a m, Audio m) => m ()
playTransition = do
  PlayVars{pvUpcomingObstacles} <- gets (view playVars)
  modify $ playVars .~ (initPlayVars pvUpcomingObstacles)
  playGameMusic

toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

mainLoop :: (MonadReader Config m, MonadState Vars m, Audio m, Logger m, Clock m, Renderer m, HasInput m, Title m, Play m, Pause m) => m ()
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
  when (nextScene /= scene) $ do
    case nextScene of
      Scene'Title -> titleTransition
      Scene'Play -> case scene of
        Scene'Title -> playTransition
        _ -> return ()
      Scene'Pause -> return ()
      Scene'GameOver -> return ()
      Scene'Quit -> return ()
    modify (\v -> v { vScene = nextScene })
  unless quit mainLoop
