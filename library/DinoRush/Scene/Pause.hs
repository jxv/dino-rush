module DinoRush.Scene.Pause where

import Control.Monad (when)
import Control.Monad.State (MonadState)
import KeyState

import DinoRush.Effect.Renderer
import DinoRush.Effect.Camera
import DinoRush.Engine.Input
import DinoRush.Scene.Play
import DinoRush.Engine.Play
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

class Monad m => Pause m where
  pauseStep :: m ()

pauseStep' :: (HasPlayVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m) => m ()
pauseStep' = do
  input <- getInput
  drawPlay
  drawPause
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)

drawPause :: (Renderer m, CameraControl m) => m ()
drawPause = do
  drawBlackOverlay 0.5
  enableHUD
  drawPauseText (540,330)
  disableHUD
