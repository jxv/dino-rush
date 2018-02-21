module DinoRush.Scene.GameOver where

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

class Monad m => GameOver m where
  gameOverStep :: m ()

gameOverStep' :: (HasPlayVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m) => m ()
gameOverStep' = do
  input <- getInput
  drawPlay
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Title)
