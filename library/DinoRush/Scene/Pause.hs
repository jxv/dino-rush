module DinoRush.Scene.Pause where

import Control.Monad (when)
import KeyState

import DinoRush.Input
import DinoRush.Scene
import DinoRush.Scene.Play

pauseStep :: (SceneManager m, HasInput m) => m ()
pauseStep = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
