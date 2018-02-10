module DinoRush.Scene.Pause where

import qualified Animate
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.State (MonadState, gets)
import KeyState

import DinoRush.Config
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Scene.Play
import DinoRush.Manager.Scene

class Monad m => Pause m where
  pauseStep :: m ()

pauseStep' :: (HasPlayVars s, MonadState s m, SceneManager m, HasInput m, Renderer m) => m ()
pauseStep' = do
  input <- getInput
  drawPlay
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
