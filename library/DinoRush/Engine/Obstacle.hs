module DinoRush.Engine.Obstacle where

import qualified Animate
import System.Random

import DinoRush.Engine.Types
import DinoRush.Engine.Bird
import DinoRush.Engine.Bouncer
import DinoRush.Engine.Lava
import DinoRush.Engine.Rock
import DinoRush.Engine.Physics

data ObstacleTag
  = ObstacleTag'Lava
  | ObstacleTag'Rock
  | ObstacleTag'Bird
  | ObstacleTag'Bouncer
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Random ObstacleTag where
  randomR = randomRBoundedEnum
  random g = randomR (minBound, maxBound) g

data ObstacleInfo
  = ObstacleInfo'Lava (Animate.Position LavaKey Seconds)
  | ObstacleInfo'Rock (Animate.Position RockKey Seconds)
  | ObstacleInfo'Bird (Animate.Position BirdKey Seconds)
  | ObstacleInfo'Bouncer Percent (Animate.Position BouncerKey Seconds)
  deriving (Show, Eq)

data ObstacleState = ObstacleState
  { osInfo :: ObstacleInfo
  , osDistance :: Distance
  } deriving (Show, Eq)

randomRBoundedEnum :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRBoundedEnum (aMin, aMax) g = let
  (index, g') = randomR (fromEnum aMin, fromEnum aMax) g
  lastEnum = maxBound
  a = [minBound..lastEnum] !! (index `mod` fromEnum lastEnum)
  in (a, g')

streamOfObstacles :: RandomGen g => g -> [(Int, ObstacleTag)]
streamOfObstacles g = zip (map (\dist -> dist `mod` 18 + 3) $ randoms g) (randoms g)

stepObstacles :: Distance -> [ObstacleState] -> [ObstacleState]
stepObstacles delta = map
  (\o@ObstacleState{osInfo, osDistance} -> o { osDistance = osDistance - (case osInfo of ObstacleInfo'Bird _ -> delta * 2; _ -> delta) })

removeOutOfBoundObstacles :: [ObstacleState] -> ([ObstacleState], [ObstacleState])
removeOutOfBoundObstacles os = foldr
  (\o@ObstacleState{osDistance} (removed, remained) ->
    if inBounds osDistance
      then (o : removed, remained)
      else (removed, o : remained))
  ([], [])
  os
  where
    inBounds x = x + 32 < 0

placeObstacle  :: (Int, ObstacleTag) -> ObstacleState
placeObstacle (idxDist, obsTag) = ObstacleState
  { osInfo = case obsTag of
      ObstacleTag'Lava -> ObstacleInfo'Lava (Animate.initPosition LavaKey'Idle)
      ObstacleTag'Rock -> ObstacleInfo'Rock (Animate.initPosition RockKey'Idle)
      ObstacleTag'Bird -> ObstacleInfo'Bird (Animate.initPosition BirdKey'Idle)
      ObstacleTag'Bouncer -> ObstacleInfo'Bouncer 0 (Animate.initPosition BouncerKey'Idle)
  , osDistance = realToFrac (idxDist * 32) + realToFrac arenaWidth
  }

lastObstacleDistance :: [ObstacleState] -> Distance
lastObstacleDistance os = maximum $ (realToFrac arenaWidth - 1) : map osDistance os

canAddObstacle :: Distance -> Bool
canAddObstacle dist = dist < realToFrac arenaWidth
