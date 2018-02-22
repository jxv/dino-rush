module DinoRush.Scene.Death where

import qualified Animate
import Control.Monad (when)
import Control.Lens (view)
import Control.Monad.State (MonadState, gets)

import DinoRush.Effect.Renderer
import DinoRush.Effect.HUD
import DinoRush.Effect.Camera
import DinoRush.Engine.Dino
import DinoRush.Engine.Frame
import DinoRush.Engine.Common
import DinoRush.Scene.Play
import DinoRush.Engine.Play
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

class Monad m => Death m where
  deathStep :: m ()

deathStep' :: (HasPlayVars s, HasCommonVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m, HUD m) => m ()
deathStep' = do
  updateDeath
  drawPlay
  height <- gets (dsHeight . pvDinoState . view playVars)
  case height of
    Nothing -> return ()
    Just h -> when (h > pi / 2) (toScene Scene'GameOver)

updateDeath :: (HasPlayVars s, MonadState s m, Renderer m) => m ()
updateDeath = do
  animations <- getDinoAnimations
  modifyPlayVars $ \pv -> let
    ds = pvDinoState pv
    ds' = ds { dsHeight = maybe (Just 0) (\x -> Just $ x + 0.03) (dsHeight ds) }
    in pv { pvDinoState = ds', pvDinoPos = Animate.stepPosition animations (pvDinoPos pv) frameDeltaSeconds }
