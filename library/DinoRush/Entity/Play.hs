{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Entity.Play where

import qualified Animate
import Control.Lens

import DinoRush.Engine.Types
import DinoRush.Entity.Dino
import DinoRush.Entity.Mountain
import DinoRush.Entity.Obstacle

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
  , pvGroundPosition :: Percent
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
  , pvGroundPosition = 0
  , pvNeargroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }
