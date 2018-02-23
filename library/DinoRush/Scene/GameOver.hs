module DinoRush.Scene.GameOver where

import Control.Monad (when)
import Control.Monad.State (MonadState, gets, modify)
import Control.Lens ((%~), view)
import KeyState

import DinoRush.Effect.Renderer
import DinoRush.Effect.Camera
import DinoRush.Effect.HUD
import DinoRush.Engine.Input
import DinoRush.Engine.Common
import DinoRush.Scene.Play
import DinoRush.Engine.Play
import DinoRush.Engine.GameOver
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

class Monad m => GameOver m where
  gameOverStep :: m ()

gameOverStep' :: (HasPlayVars s, HasCommonVars s, HasGameOverVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m, HUD m) => m ()
gameOverStep' = do
  input <- getInput
  updateGameOver
  drawPlay
  drawGameOver
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Title)

updateGameOver :: (HasGameOverVars s, HasPlayVars s, MonadState s m) => m ()
updateGameOver = modify $ gameOverVars %~ stepGameOverVars

drawGameOver :: (Renderer m, CameraControl m, MonadState s m, HasGameOverVars s) => m ()
drawGameOver = do
  gov <- gets (view gameOverVars)
  drawBlackOverlay (govFadeout gov)
  disableZoom
  drawGameOverText (470,300)
  when (gameOverShowPressSpace $ govSpaceFlashing gov) $ drawPressSpaceText (550,500)
  enableZoom
