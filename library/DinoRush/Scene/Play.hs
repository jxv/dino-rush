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
      ObstacleInfo'Lava pos -> drawLava (Animate.currentLocation lavaAnimations pos) (x, lavaY)
      ObstacleInfo'Rock pos -> drawRock (Animate.currentLocation rockAnimations pos) (x, rockY)
      ObstacleInfo'Bird pos -> drawBird (Animate.currentLocation birdAnimations pos) (x, birdY)
      ObstacleInfo'Bouncer percentY pos -> drawBouncer (Animate.currentLocation bouncerAnimations pos) (x, truncate percentY + bouncerY)

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

applyHurt :: Bool -> Step DinoAction -> Maybe Percent -> Step DinoAction
applyHurt collision stepDa recover
  | collision && recover == Nothing = case stepDa of
      Step'Sustain DinoAction'Hurt -> stepDa
      Step'Sustain da -> Step'Change da DinoAction'Hurt
      Step'Change da _ -> Step'Change da DinoAction'Hurt
  | otherwise = stepDa

detectCollision :: [ObstacleState] -> DinoState -> Bool
detectCollision obstacles dinoState = or $ flip map obstacles $ \obs ->
  collisionIntersect (dinoAabb (dsHeight dinoState)) (obstacleAabb obs)

modifyPlayVars :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = modify $ playVars %~ f

clearSfx :: (MonadState s m, HasPlayVars s) => m ()
clearSfx = modifyPlayVars $ \pv -> pv { pvSfx = [] }

updateSpeed :: (MonadState s m, HasPlayVars s) => Step DinoAction -> m ()
updateSpeed da = modifyPlayVars $ \pv -> pv { pvSpeed = stepSpeed da (pvSpeed pv) }

updateDino :: (MonadState s m, HasPlayVars s, Renderer m) => Step DinoAction -> m ()
updateDino da = do
  dinoAnimations <- getDinoAnimations
  let sfx = case da of
        Step'Sustain _ -> []
        Step'Change _ da' -> case da' of
          DinoAction'Jump -> [Sfx'Jump]
          DinoAction'Duck -> [Sfx'Duck]
          DinoAction'Hurt -> [Sfx'Hurt]
          _ -> []
  modifyPlayVars $ \pv -> pv
    { pvDinoState = stepDinoState da (pvDinoState pv)
    , pvSfx = pvSfx pv ++ sfx
    , pvDinoPos = stepDinoPosition da dinoAnimations (pvDinoPos pv)
    }

updateZoom :: (MonadState s m, HasPlayVars s) => Step DinoAction -> m ()
updateZoom da = modifyPlayVars $ \pv -> pv { pvZoom = stepZoom (pvZoom pv) (smash da) }

updateCamera :: (MonadState s m, HasPlayVars s, CameraControl m) => m ()
updateCamera = do
  zoom <- gets (pvZoom . view playVars)
  adjustCamera $ lerpCamera ((1 - zoom) ** (1.8 :: Float)) duckCamera initCamera

updateObstacles :: (MonadState s m, HasPlayVars s) => m ()
updateObstacles = do
  PlayVars{pvUpcomingObstacles,pvObstacles,pvSpeed} <- gets (view playVars)
  let (obstacles, removedCount, upcomingObstacles, newObstacleTag) = iterateObstacles pvUpcomingObstacles pvSpeed pvObstacles
  let pointSfx = if removedCount > 0 then [Sfx'Point] else []
  let obstacleSfx = case newObstacleTag of
        Nothing -> []
        Just o -> case o of
          ObstacleTag'Lava -> [Sfx'Lava]
          ObstacleTag'Rock -> [Sfx'Rock]
          ObstacleTag'Bird -> [Sfx'Bird]
          ObstacleTag'Bouncer -> [Sfx'Bouncer]
  modifyPlayVars $ \pv -> pv
    { pvObstacles = obstacles
    , pvSfx = pvSfx pv ++ pointSfx ++ obstacleSfx
    , pvScore = pvScore pv + fromIntegral removedCount
    , pvUpcomingObstacles = upcomingObstacles
    }

tryCollision :: (MonadState s m, HasPlayVars s) => Step DinoAction -> m (Bool, Step DinoAction)
tryCollision da = do
  pv <- gets (view playVars)
  let collision = detectCollision (pvObstacles pv) (pvDinoState pv) && dsRecover (pvDinoState pv) == Nothing
  let da' = applyHurt collision da (dsRecover (pvDinoState pv))
  return (collision, da')

updateScrolling :: (Renderer m, HasPlayVars s, MonadState s m) => m ()
updateScrolling = do
  mountainAnimations <- getMountainAnimations
  modifyPlayVars $ \pv -> let
    speed = pvSpeed pv
    in pv
      { pvMountainPos = Animate.stepPosition mountainAnimations (pvMountainPos pv) frameDeltaSeconds
      , pvMountainScroll = stepHorizontalDistance (realToFrac $ pvMountainScroll pv) (realToFrac (-speed) / 3)
      , pvJungleScroll = stepHorizontalDistance (realToFrac $ pvJungleScroll pv) (realToFrac (-speed) / 2)
      , pvGroundScroll = stepHorizontalDistance (realToFrac $ pvGroundScroll pv) (realToFrac (-speed))
      , pvRiverScroll = stepHorizontalDistance (realToFrac $ pvRiverScroll pv) (realToFrac (-speed) * 1.5)
      }

updateLives :: (MonadState s m, HasPlayVars s) => Bool -> m ()
updateLives collision = modifyPlayVars $ \pv -> pv { pvLives = pvLives pv - (if collision then 1 else 0) }

getDead :: (MonadState s m, HasPlayVars s) => m Bool
getDead = (<= 0) <$> gets (pvLives . view playVars)

updatePlay :: (HasPlayVars s, MonadState s m, Logger m, Clock m, CameraControl m, Renderer m, HasInput m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  clearSfx
  da <- (stepDinoAction input . pvDinoState) <$> gets (view playVars)
  updateSpeed da
  updateObstacles
  (collision, da') <- tryCollision da
  updateZoom da'
  updateDino da'
  updateCamera
  updateScrolling
  updateLives collision
  isDead <- getDead
  when isDead (toScene Scene'Death)
