{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Play where

import qualified Animate
import Control.Monad (when)
import Control.Lens
import Control.Monad.State (MonadState(..), modify, gets)
import Data.Foldable (forM_)
import KeyState

import DinoRush.Effect.Audio
import DinoRush.Effect.Clock
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Types
import DinoRush.Engine.Step
import DinoRush.Engine.Dino
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Play
import DinoRush.Manager.Scene
import DinoRush.Manager.Input

class Monad m => Play m where
  playStep :: m ()

stepHorizontal :: Percent -> Percent -> Percent
stepHorizontal percent speed = if percent' <= -1 then percent' + 1 else percent'
  where
    percent' = percent - speed

stepHorizontalDistance :: Distance -> Distance -> Distance
stepHorizontalDistance dist speed = if dist' <= -1280 then dist' + 1280 else dist'
  where
    dist' = dist + speed

drawPlay :: (HasPlayVars s, MonadState s m, Renderer m) => m ()
drawPlay = do
  dinoAnimations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  pv <- gets (view playVars)
  let dinoLoc = Animate.currentLocation dinoAnimations (pvDinoPos pv)
  let mountainLoc = Animate.currentLocation mountainAnimations (pvMountainPos pv)
  drawMountain mountainLoc (truncate $ pvMountainScroll pv, mountainY)
  drawJungle (truncate $ pvJungleScroll pv, jungleY)
  drawGround (truncate $ pvGroundScroll pv, groundY)
  drawDino dinoLoc (200, dinoHeight (pvDinoAction pv))
  drawObstacles (pvObstacles pv)
  drawRiver (truncate $ pvRiverScroll pv, riverY)

drawObstacles :: Renderer m => [ObstacleState] -> m ()
drawObstacles obstacles = do
  lavaAnimations <- getLavaAnimations
  rockAnimations <- getRockAnimations
  birdAnimations <- getBirdAnimations
  bouncerAnimations <- getBouncerAnimations
  forM_ obstacles $ \ObstacleState{osInfo,osDistance} -> let
    x = truncate osDistance
    in case osInfo of
      ObstacleInfo'Lava pos -> drawLava (Animate.currentLocation lavaAnimations pos) (x, 16 * 28)
      ObstacleInfo'Rock pos -> drawRock (Animate.currentLocation rockAnimations pos) (x, 16 * 26)
      ObstacleInfo'Bird pos -> drawBird (Animate.currentLocation birdAnimations pos) (x, 16 * 22)
      ObstacleInfo'Bouncer percentY pos -> drawBouncer (Animate.currentLocation bouncerAnimations pos) (x, truncate percentY + 16 * 26)

playStep' :: (HasPlayVars s, MonadState s m, Logger m, Clock m, Renderer m, Audio m, HasInput m, SceneManager m) => m ()
playStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  sfxPlay
  drawPlay

sfxPlay :: (Audio m, HasPlayVars s, MonadState s m) => m ()
sfxPlay = do
  PlayVars{pvDinoSfx} <- gets (view playVars)
  forM_ pvDinoSfx $ \sfx -> case sfx of
    DinoSfx'Jump -> playJumpSfx

updatePlay :: (HasPlayVars s, MonadState s m, Logger m, Clock m, Renderer m, HasInput m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  dinoAnimations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  pv' <- gets (view playVars)
  let dinoAction = stepDinoAction input (pvDinoAction pv')
  let (removed, remained) = removeOutOfBoundObstacles $ stepObstacles (realToFrac (pvSpeed pv')) (pvObstacles pv')
  let (upcomingObstacles, obstacles) = if canAddObstacle (lastObstacleDistance remained)
        then (tail $ pvUpcomingObstacles pv', (placeObstacle $ head $ pvUpcomingObstacles pv') : remained)
        else (pvUpcomingObstacles pv', remained)
  let speed = stepSpeed dinoAction (pvSpeed pv')
  modify $ playVars %~ (\pv -> pv
    { pvDinoPos = stepDinoPosition dinoAction dinoAnimations (pvDinoPos pv)
    , pvMountainPos = Animate.stepPosition mountainAnimations (pvMountainPos pv) frameDeltaSeconds
    , pvMountainScroll = stepHorizontalDistance (realToFrac $ pvMountainScroll pv) (realToFrac (-speed) / 3)
    , pvJungleScroll = stepHorizontalDistance (realToFrac $ pvJungleScroll pv) (realToFrac (-speed) / 2)
    , pvGroundScroll = stepHorizontalDistance (realToFrac $ pvGroundScroll pv) (realToFrac (-speed))
    , pvRiverScroll = stepHorizontalDistance (realToFrac $ pvRiverScroll pv) (realToFrac (-speed) * 1.5)
    , pvSpeed = speed
    , pvDinoAction = smash dinoAction
    , pvDinoSfx = stepDinoSfx dinoAction
    , pvObstacles = obstacles
    , pvScore = pvScore pv + fromIntegral (length removed)
    , pvUpcomingObstacles = upcomingObstacles
    })
