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
import DinoRush.Effect.HUD
import DinoRush.Effect.Renderer
import DinoRush.Effect.Sfx
import DinoRush.Engine.Common
import DinoRush.Engine.Input
import DinoRush.Engine.Camera
import DinoRush.Engine.Frame
import DinoRush.Engine.Step
import DinoRush.Engine.Dino
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Play
import DinoRush.Engine.Sfx
import DinoRush.Engine.Quake
import DinoRush.Engine.Physics
import DinoRush.Manager.Scene
import DinoRush.Manager.Input

class Monad m => Play m where
  playStep :: m ()

playStep' :: (HasPlayVars s, HasCommonVars s, MonadState s m, Logger m, CameraControl m, Clock m, Renderer m, Audio m, AudioSfx m, HasInput m, SceneManager m, HUD m) => m ()
playStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  drawPlay

updatePlay :: (HasPlayVars s, HasCommonVars s, MonadState s m, Logger m, Clock m, CameraControl m, Renderer m, HasInput m, AudioSfx m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  da <- (stepDinoAction input . pvDinoState) <$> gets (view playVars)
  updateSpeed da
  updateObstacles
  (collision, da') <- tryCollision da
  updateZoom da'
  updateDino da'
  updateCamera
  updateScrolling
  updateStocks collision
  updateHiscore
  isDead <- getDead
  when isDead (toScene Scene'Death)

drawPlay :: (HasPlayVars s, HasCommonVars s, MonadState s m, Renderer m, CameraControl m, HUD m) => m ()
drawPlay = do
  dinoAnimations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  riverAnimations <- getRiverAnimations
  quake <- gets (cvQuake . view commonVars)
  pv <- gets (view playVars)
  let dinoLoc = Animate.currentLocation dinoAnimations (pvDinoPos pv)
  let mountainLoc = Animate.currentLocation mountainAnimations (pvMountainPos pv)
  let riverLoc = Animate.currentLocation riverAnimations (pvRiverPos pv)
  drawMountain mountainLoc $ applyQuakeToMountain quake (truncate $ pvMountainScroll pv, mountainY)
  drawJungle $ applyQuakeToJungle quake (truncate $ pvJungleScroll pv, jungleY)
  drawGround $ applyQuakeToGround quake (truncate $ pvGroundScroll pv, groundY)
  when (pvShowDino pv) $ drawDino dinoLoc $ applyQuakeToGround quake (truncate dinoX, dinoHeight (dsHeight $ pvDinoState pv))
  drawObstacles quake (pvObstacles pv)
  drawRiver riverLoc $ applyQuakeToRiver quake (truncate $ pvRiverScroll pv, riverY)
  disableZoom
  drawStocks pv dinoAnimations
  drawHiscore
  drawScore
  enableZoom
  where
    drawStocks pv dinoAnimations =
      flip mapM_ [1..(fromIntegral $ pvStocks pv)] $ \stock -> do
        let idleLoc = Animate.currentLocation dinoAnimations (Animate.initPosition DinoKey'Kick)
        drawDino idleLoc (20 + 48 * (stock - 1), 32)

drawObstacles :: Renderer m => Quake -> [ObstacleState] -> m ()
drawObstacles quake obstacles = do
  lavaAnimations <- getLavaAnimations
  rockAnimations <- getRockAnimations
  birdAnimations <- getBirdAnimations
  forM_ obstacles $ \ObstacleState{osInfo,osDistance} -> let
    x = truncate osDistance
    in case osInfo of
      ObstacleInfo'Lava pos -> drawLava (Animate.currentLocation lavaAnimations pos) $ applyQuakeToGround quake (x, lavaY)
      ObstacleInfo'Rock pos -> drawRock (Animate.currentLocation rockAnimations pos) $ applyQuakeToGround quake (x, rockY)
      ObstacleInfo'Bird pos -> drawBird (Animate.currentLocation birdAnimations pos) $ applyQuakeToGround quake (x, birdY)

detectCollision :: [ObstacleState] -> DinoState -> Bool
detectCollision obstacles dinoState = or $ flip map obstacles $ \obs ->
  collisionIntersect (dinoAabb (dsHeight dinoState)) (obstacleAabb obs)

modifyPlayVars :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = modify $ playVars %~ f

updateSpeed :: (MonadState s m, HasPlayVars s) => Step DinoAction -> m ()
updateSpeed da = modifyPlayVars $ \pv -> pv { pvSpeed = stepSpeed da (pvSpeed pv) }

updateDino :: (MonadState s m, HasPlayVars s, Renderer m, AudioSfx m) => Step DinoAction -> m ()
updateDino sda = do
  dinoAnimations <- getDinoAnimations
  let sfx = case sda of
        Step'Sustain _ -> []
        Step'Change da da' -> case da' of
          DinoAction'Jump -> [Sfx'Jump]
          DinoAction'Duck -> [Sfx'Duck]
          DinoAction'Hurt -> [Sfx'Hurt]
          DinoAction'Move -> case da of
            DinoAction'Hurt -> [Sfx'Recover]
            _ -> []
  addSfxs sfx
  modifyPlayVars $ \pv -> let
    ds = stepDinoState sda (pvDinoState pv)
    in pv
      { pvDinoState = ds
      , pvShowDino = showDino ds
      , pvDinoPos = stepDinoPosition sda dinoAnimations (pvDinoPos pv)
      }

updateZoom :: (MonadState s m, HasPlayVars s) => Step DinoAction -> m ()
updateZoom da = modifyPlayVars $ \pv -> pv { pvZoom = stepZoom (pvZoom pv) (smash da) }

updateCamera :: (MonadState s m, HasPlayVars s, CameraControl m) => m ()
updateCamera = do
  zoom <- gets (pvZoom . view playVars)
  let cam = lerpCamera ((1 - zoom) ** (1.8 :: Float)) duckCamera initCamera
  adjustCamera cam

updateObstacles :: (MonadState s m, HasPlayVars s, AudioSfx m) => m ()
updateObstacles = do
  PlayVars{pvUpcomingObstacles,pvObstacles,pvSpeed,pvScore} <- gets (view playVars)
  let (obstacles, removedCount, upcomingObstacles, newObstacleTag) = iterateObstacles pvUpcomingObstacles pvSpeed pvObstacles
  let pointSfx = if removedCount > 0 then [Sfx'Point] else []
  let obstacleSfx = case newObstacleTag of
        Nothing -> []
        Just o -> case o of
          ObstacleTag'Lava -> [Sfx'Lava]
          ObstacleTag'Rock -> [Sfx'Rock]
          ObstacleTag'Bird -> [Sfx'Bird]
  let score = pvScore + fromIntegral removedCount
  let stockSfx = if addStocks pvScore score then [Sfx'Stock] else []
  addSfxs $ pointSfx ++ obstacleSfx ++ stockSfx
  modifyPlayVars $ \pv -> let
    in pv
      { pvObstacles = obstacles
      , pvScore = score
      , pvUpcomingObstacles = upcomingObstacles
      , pvStocks = nextStocks pvScore score (pvStocks pv)
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
  riverAnimations <- getRiverAnimations
  modifyPlayVars $ \pv -> let
    speed = pvSpeed pv
    in pv
      { pvMountainPos = Animate.stepPosition mountainAnimations (pvMountainPos pv) frameDeltaSeconds
      , pvRiverPos = Animate.stepPosition riverAnimations (pvRiverPos pv) frameDeltaSeconds
      , pvMountainScroll = stepHorizontalDistance (realToFrac $ pvMountainScroll pv) (realToFrac (-speed) / 3)
      , pvJungleScroll = stepHorizontalDistance (realToFrac $ pvJungleScroll pv) (realToFrac (-speed) / 2)
      , pvGroundScroll = stepHorizontalDistance (realToFrac $ pvGroundScroll pv) (realToFrac (-speed))
      , pvRiverScroll = stepHorizontalDistance (realToFrac $ pvRiverScroll pv) (realToFrac (-speed) * 1.5)
      }

updateStocks :: (MonadState s m, HasPlayVars s) => Bool -> m ()
updateStocks collision = modifyPlayVars $ \pv -> pv { pvStocks = pvStocks pv - (if collision then 1 else 0) }

updateHiscore :: (MonadState s m, HasCommonVars s, HasPlayVars s) => m ()
updateHiscore = do
  score <- gets (pvScore . view playVars)
  modify $ commonVars %~ \cv -> cv { cvHiscore = max (cvHiscore cv) score }

getDead :: (MonadState s m, HasPlayVars s) => m Bool
getDead = (<= 0) <$> gets (pvStocks . view playVars)
