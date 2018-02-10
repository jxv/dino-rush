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
import DinoRush.Engine.Types
import DinoRush.Entity.Dino
import DinoRush.Manager.Scene

data PlayVars = PlayVars
  { pvScore :: Score
  , pvLives :: Lives
  , pvSpeed :: Percent
  , pvProgress :: Distance
  , pvDinoPos :: Animate.Position DinoKey Seconds
  , pvDinoSfx :: [DinoSfx]
  , pvMountainPos :: Animate.Position MountainKey Seconds
  , pvDinoAction :: DinoAction
  , pvMountainScroll :: Percent
  , pvBackgroundPositionNear :: Percent
  , pvForegroundPosition :: Percent
  , pvNeargroundPosition :: Percent
  , pvObstacles :: [ObstacleState]
  , pvUpcomingObstacles :: [(Distance, ObstacleTag)]
  } deriving (Show, Eq)

makeClassy ''PlayVars

initPlayVars :: [(Distance, ObstacleTag)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvLives = 1
  , pvSpeed = 1
  , pvProgress = 0
  , pvDinoAction = DinoAction'Move
  , pvDinoPos = Animate.initPosition DinoKey'Move
  , pvDinoSfx = []
  , pvMountainPos = Animate.initPosition MountainKey'Idle
  , pvMountainScroll = 0
  , pvBackgroundPositionNear = 0
  , pvForegroundPosition = 0
  , pvNeargroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }

class Monad m => Play m where
  playStep :: m ()

stepHorizontal :: Percent -> Percent -> Percent
stepHorizontal percent speed = if percent' <= -1 then percent' + 1 else percent'
  where
    percent' = percent - speed

drawPlay :: (HasPlayVars s, MonadState s m, Renderer m) => m ()
drawPlay = do
  dinoAnimations <- getDinoAnimations
  mountainAnimations <- getMountainAnimations
  pv <- gets (view playVars)
  let dinoLoc = Animate.currentLocation dinoAnimations (pvDinoPos pv)
  let mountainLoc = Animate.currentLocation mountainAnimations (pvMountainPos pv)
  drawMountain mountainLoc (truncate $ 1280 * pvMountainScroll pv, mountainY)
  drawBackgroundNear (truncate $ 1280 * pvBackgroundPositionNear pv, backgroundNearY)
  drawForeground (truncate $ 1280 * pvForegroundPosition pv, foregroundY)
  drawDino dinoLoc (200, dinoHeight (pvDinoAction pv))
  drawNearground (truncate $ 1280 * pvNeargroundPosition pv, neargroundY)

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
  PlayVars{pvDinoAction} <- gets (view playVars)
  let dinoAction = stepDinoAction input pvDinoAction
  modify $ playVars %~ (\pv -> pv
    { pvDinoPos = stepDinoPosition dinoAction dinoAnimations (pvDinoPos pv)
    , pvMountainPos = Animate.stepPosition mountainAnimations (pvMountainPos pv) frameDeltaSeconds
    , pvMountainScroll = stepHorizontal (pvMountainScroll pv) (pvSpeed pv * 0.003)
    , pvBackgroundPositionNear = stepHorizontal (pvBackgroundPositionNear pv) (pvSpeed pv * 0.006)
    , pvForegroundPosition = stepHorizontal (pvForegroundPosition pv) (pvSpeed pv * 0.009)
    , pvNeargroundPosition = stepHorizontal (pvNeargroundPosition pv) (pvSpeed pv * 0.012)
    , pvSpeed = clamp (pvSpeed pv + 0.01) 5
    , pvDinoAction = smash dinoAction
    , pvDinoSfx = stepDinoSfx dinoAction
    })
