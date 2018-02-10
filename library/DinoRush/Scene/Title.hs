{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Title where

import qualified SDL
import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Linear
import KeyState

import DinoRush.Input
import DinoRush.Renderer
import DinoRush.Scene
import DinoRush.Types

import DinoRush.SDL.Renderer

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  , tvMountainPos :: Animate.Position MountainKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle) (Animate.initPosition MountainKey'Idle)

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
titleStep' = do
  input <- getInput
  animations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  pos <- gets (tvPlayer . view titleVars)
  mountainPos <- gets (tvMountainPos . view titleVars)
  let pos' = Animate.stepPosition animations pos frameDeltaSeconds
  let loc = Animate.currentLocation animations pos'
  let mountainPos' = Animate.stepPosition mountainAnimations mountainPos frameDeltaSeconds
  let mountainLoc = Animate.currentLocation mountainAnimations mountainPos'
  drawMountain mountainLoc (0, mountainY)
  drawBackgroundNear (0, backgroundNearY)
  drawForeground (0, foregroundY)
  drawDino loc (200, dinoY)
  drawNearground (0, neargroundY)
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos', tvMountainPos = mountainPos' })
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
