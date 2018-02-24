{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.Play where

import qualified Animate
import Control.Lens

import DinoRush.Engine.Dino
import DinoRush.Engine.Mountain
import DinoRush.Engine.River
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvStocks :: Stocks
  , pvSpeed :: Percent
  , pvSeconds :: Seconds
  , pvZoom :: Float
  , pvShowDino :: Bool
  , pvDinoPos :: Animate.Position DinoKey Seconds
  , pvMountainPos :: Animate.Position MountainKey Seconds
  , pvRiverPos :: Animate.Position RiverKey Seconds
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
  , pvStocks = 3
  , pvSeconds = 0
  , pvSpeed = 1
  , pvShowDino = True
  , pvZoom = 1
  , pvDinoState = DinoState DinoAction'Move Nothing Nothing Nothing
  , pvDinoPos = Animate.initPosition DinoKey'Move
  , pvMountainPos = Animate.initPosition MountainKey'Idle
  , pvRiverPos = Animate.initPosition RiverKey'Idle
  , pvMountainScroll = 0
  , pvJungleScroll = 0
  , pvGroundScroll = 0
  , pvRiverScroll = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }
