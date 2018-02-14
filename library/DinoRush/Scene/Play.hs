{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Play where

import qualified Animate
import Control.Monad (when)
import Control.Lens hiding (zoom)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.Foldable (forM_)
import KeyState

import DinoRush.Effect.Audio
import DinoRush.Effect.Clock
import DinoRush.Effect.Camera
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Engine.Camera
import DinoRush.Engine.Frame
import DinoRush.Engine.Types
import DinoRush.Engine.Step
import DinoRush.Engine.Dino
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Play
import DinoRush.Engine.Sfx
import DinoRush.Engine.Physics
import DinoRush.Manager.Scene
import DinoRush.Manager.Input

class Monad m => Play m where
  playStep :: m ()

stepHorizontalDistance :: Distance -> Distance -> Distance
stepHorizontalDistance dist speed = if dist' <= -1280 then dist' + 1280 else dist'
  where
    dist' = dist + speed

stepSfx :: Step DinoAction -> Bool -> Maybe ObstacleTag -> [Sfx]
stepSfx dinoAction point obstacle = dinoSfx ++ pointSfx ++ obstacleSfx
  where
    pointSfx = if point then [Sfx'Point] else []
    dinoSfx = case dinoAction of
      Step'Sustain _ -> []
      Step'Change _ da -> case da of
        DinoAction'Jump -> [Sfx'Jump]
        DinoAction'Duck -> [Sfx'Duck]
        DinoAction'Hurt -> [Sfx'Hurt]
        _ -> []
    obstacleSfx = case obstacle of
      Nothing -> []
      Just o -> case o of
        ObstacleTag'Lava -> [Sfx'Lava]
        ObstacleTag'Rock -> [Sfx'Rock]
        ObstacleTag'Bird -> [Sfx'Bird]
        ObstacleTag'Bouncer -> [Sfx'Bouncer]

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
  drawDino dinoLoc (truncate dinoX, dinoHeight (dsHeight $ pvDinoState pv))
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

playStep' :: (HasPlayVars s, MonadState s m, Logger m, CameraControl m, Clock m, Renderer m, Audio m, HasInput m, SceneManager m) => m ()
playStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  sfxPlay
  drawPlay

sfxPlay :: (Audio m, HasPlayVars s, MonadState s m) => m ()
sfxPlay = do
  PlayVars{pvSfx} <- gets (view playVars)
  forM_ pvSfx $ \sfx -> case sfx of
    Sfx'Jump -> playJumpSfx
    Sfx'Duck -> playDuckSfx
    Sfx'Point -> playPointSfx
    Sfx'Bird -> playBirdSfx
    Sfx'Bouncer -> playBouncerSfx
    Sfx'Hurt -> playHurtSfx
    Sfx'Lava -> playLavaSfx
    Sfx'Quake -> playQuakeSfx
    Sfx'Rock -> playRockSfx

stepZoom :: Float -> DinoAction -> Float
stepZoom zoom dinoAction = case dinoAction of
  DinoAction'Duck -> clamp (zoom - 0.01) 0 1
  _ -> clamp (zoom + 0.05) 0 1

iterateObstacles :: [(Int, ObstacleTag)] -> Percent -> [ObstacleState] -> ([ObstacleState], Int, [(Int, ObstacleTag)], Maybe ObstacleTag)
iterateObstacles upcomingObstacles speed obstacles = let
  (removed, remained) = removeOutOfBoundObstacles $ stepObstacles (realToFrac speed) obstacles
  newObstacle = if canAddObstacle (lastObstacleDistance remained)
    then let
      pair = head $ upcomingObstacles
      in Just (snd pair, placeObstacle pair)
    else Nothing
  (upcomingObstacles', obstacles') = case fmap snd newObstacle of
    Nothing -> (upcomingObstacles, remained)
    Just obstacle -> (tail $ upcomingObstacles, obstacle : remained)
  in (obstacles', length removed, upcomingObstacles', fmap fst newObstacle)

applyHurt :: Bool -> Step DinoAction -> Step DinoAction
applyHurt collision stepDa
  | collision = case stepDa of
      Step'Sustain DinoAction'Hurt -> stepDa
      Step'Sustain da -> Step'Change da DinoAction'Hurt
      Step'Change da _ -> Step'Change da DinoAction'Hurt
  | otherwise = stepDa

detectCollision :: [ObstacleState] -> DinoState -> Bool
detectCollision obstacles dinoState = or $ flip map obstacles $ \obs -> collisionIntersect (dinoAabb (dsHeight dinoState)) (obstacleAabb obs)

updatePlay :: (HasPlayVars s, MonadState s m, Logger m, Clock m, CameraControl m, Renderer m, HasInput m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  dinoAnimations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  pv' <- gets (view playVars)
  let dinoAction' = stepDinoAction input (pvDinoState pv')
  let speed = stepSpeed dinoAction' (pvSpeed pv')
  let (obstacles, removedCount, upcomingObstacles, newObstacleTag) = iterateObstacles (pvUpcomingObstacles pv') speed (pvObstacles pv')
  let collision = detectCollision obstacles (pvDinoState pv')
  let dinoAction = applyHurt collision dinoAction'
  let zoom' = stepZoom (pvZoom pv') (smash dinoAction)
  let dinoState = stepDinoState dinoAction (pvDinoState pv')
  adjustCamera $ lerpCamera ((1 - zoom') ** (1.8 :: Float)) duckCamera initCamera
  modify $ playVars %~ (\pv -> pv
    { pvDinoPos = stepDinoPosition dinoAction dinoAnimations (pvDinoPos pv)
    , pvMountainPos = Animate.stepPosition mountainAnimations (pvMountainPos pv) frameDeltaSeconds
    , pvMountainScroll = stepHorizontalDistance (realToFrac $ pvMountainScroll pv) (realToFrac (-speed) / 3)
    , pvJungleScroll = stepHorizontalDistance (realToFrac $ pvJungleScroll pv) (realToFrac (-speed) / 2)
    , pvGroundScroll = stepHorizontalDistance (realToFrac $ pvGroundScroll pv) (realToFrac (-speed))
    , pvRiverScroll = stepHorizontalDistance (realToFrac $ pvRiverScroll pv) (realToFrac (-speed) * 1.5)
    , pvSpeed = speed
    , pvZoom = zoom'
    , pvDinoState = dinoState
    , pvSfx = stepSfx dinoAction (removedCount > 0) newObstacleTag
    , pvObstacles = obstacles
    , pvScore = pvScore pv + fromIntegral removedCount
    , pvUpcomingObstacles = upcomingObstacles
    })
