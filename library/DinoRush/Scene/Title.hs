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
import DinoRush.Effect.HUD
import DinoRush.Effect.Sfx
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Dino
import DinoRush.Engine.Title
import DinoRush.Engine.Common
import DinoRush.Engine.Quake
import DinoRush.Engine.Step
import DinoRush.Engine.Sfx
import DinoRush.Manager.Input
import DinoRush.Manager.Scene

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (HasTitleVars s, HasCommonVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, HUD m, AudioSfx m) => m ()
titleStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
  updateTitle
  drawTitle

updateTitle :: (HasTitleVars s, HasCommonVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, AudioSfx m) => m ()
updateTitle = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets (tvDinoPos . view titleVars)
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds

  mountainAnimations <- getMountainAnimations
  mountainPos <- gets (tvMountainPos . view titleVars)
  let mountainPos' = Animate.stepPosition mountainAnimations mountainPos frameDeltaSeconds

  sq <- stepQuake <$> gets (cvQuake . view commonVars)
  addSfxs $ if startQuake sq then [Sfx'Quake] else []
  modify $ commonVars %~ (\cv -> cv{ cvQuake = smash sq })

  modify $ titleVars %~ (\tv -> tv
    { tvDinoPos = dinoPos'
    , tvMountainPos = mountainPos'
    , tvFlashing = tvFlashing tv + 0.025
    })

drawTitle :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, HUD m) => m ()
drawTitle = do
  tv <- gets (view titleVars)

  dinoAnimations <- getDinoAnimations
  let dinoPos = tvDinoPos tv
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  drawDino dinoLoc (truncate dinoX, dinoY)

  mountainAnimations <- getMountainAnimations
  let mountainPos = tvMountainPos tv
  let mountainLoc = Animate.currentLocation mountainAnimations mountainPos
  drawMountain mountainLoc (0, mountainY)

  drawJungle (0, jungleY)
  drawGround (0, groundY)
  drawRiver (0, riverY)

  drawHiscore

  drawTitleText (300, 180)

  when (titleShowPressSpace $ tvFlashing tv) $ drawPressSpaceText (560,500)
  when (titleShowPressEscape $ tvFlashing tv) $ drawPressEscapeText (490,500)
