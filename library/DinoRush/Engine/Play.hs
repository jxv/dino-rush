{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.Play where

import qualified Animate
import Control.Lens

import DinoRush.Engine.Dino
import DinoRush.Engine.Mountain
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Sfx
import DinoRush.Engine.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvLives :: Lives
  , pvSpeed :: Percent
  , pvZoom :: Float
  , pvProgress :: Distance
  , pvShowDino :: Bool
  , pvDinoPos :: Animate.Position DinoKey Seconds
  , pvSfx :: [Sfx]
  , pvMountainPos :: Animate.Position MountainKey Seconds
  , pvDinoState :: DinoState
  , pvMountainScroll :: Distance
  , pvJungleScroll :: Distance
  , pvGroundScroll :: Distance
  , pvRiverScroll :: Distance
  , pvObstacles :: [ObstacleState]
  , pvUpcomingObstacles :: [(Int, ObstacleTag)]
  } deriving (Show, Eq)

makeClassy ''PlayVars

initPlayVars :: [(Int, ObstacleTag)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvLives = 3
  , pvSpeed = 1
  , pvShowDino = True
  , pvProgress = 0
  , pvZoom = 1
  , pvDinoState = DinoState DinoAction'Move Nothing Nothing Nothing
  , pvDinoPos = Animate.initPosition DinoKey'Move
  , pvSfx = []
  , pvMountainPos = Animate.initPosition MountainKey'Idle
  , pvMountainScroll = 0
  , pvJungleScroll = 0
  , pvGroundScroll = 0
  , pvRiverScroll = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }
