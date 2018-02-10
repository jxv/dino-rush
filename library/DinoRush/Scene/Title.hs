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
  , tvBackgroundFarPos :: Animate.Position BackgroundFarKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle) (Animate.initPosition BackgroundFarKey'Idle)

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
titleStep' = do
  input <- getInput
  animations <- getDinoAnimations
  backgroundFarAnimations <- getBackgroundFarAnimations
  pos <- gets (tvPlayer . view titleVars)
  backgroundFarPos <- gets (tvBackgroundFarPos . view titleVars)
  let pos' = Animate.stepPosition animations pos frameDeltaSeconds
  let loc = Animate.currentLocation animations pos'
  let backgroundFarPos' = Animate.stepPosition backgroundFarAnimations backgroundFarPos frameDeltaSeconds
  let backgroundFarLoc = Animate.currentLocation backgroundFarAnimations backgroundFarPos'
  drawBackgroundFar backgroundFarLoc (0, backgroundFarY)
  drawBackgroundNear (0, backgroundNearY)
  drawForeground (0, foregroundY)
  drawDino loc (200, dinoY)
  drawNearground (0, neargroundY)
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos', tvBackgroundFarPos = backgroundFarPos' })
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
