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
  , pvHiscore :: Score
  , pvStocks :: Stocks
  , pvSpeed :: Percent
  , pvZoom :: Float
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

initPlayVars :: [(Int, ObstacleTag)] -> Score -> PlayVars
initPlayVars upcomingObstacles hiscore = PlayVars
  { pvScore = 0
  , pvHiscore = hiscore
  , pvStocks = 3
  , pvSpeed = 1
  , pvShowDino = True
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
