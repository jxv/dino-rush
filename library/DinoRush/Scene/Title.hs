{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Title where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import DinoRush.Config
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Dino
import DinoRush.Engine.Title
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

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
  drawJungle (0, jungleY)
  drawGround (0, groundY)
  drawDino loc (truncate dinoX, dinoY)
  drawRiver (0, riverY)
  drawTitleText (300, 180)
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos', tvMountainPos = mountainPos' })
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
