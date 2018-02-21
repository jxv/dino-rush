module DinoRush.Scene.Death where

import Control.Monad (when)
import Control.Monad.State (MonadState)
import KeyState

import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Scene.Play
import DinoRush.Engine.Play
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

class Monad m => Death m where
  deathStep :: m ()

deathStep' :: (HasPlayVars s, MonadState s m, SceneManager m, HasInput m, Renderer m) => m ()
deathStep' = do
  input <- getInput
  drawPlay
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'GameOver)
