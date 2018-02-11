module DinoRush.Engine.Obstacle where

import qualified Animate
import System.Random

import DinoRush.Engine.Types
import DinoRush.Engine.Bird
import DinoRush.Engine.Bouncer
import DinoRush.Engine.Ground
import DinoRush.Engine.Jungle
import DinoRush.Engine.Lava
import DinoRush.Engine.Mountain
import DinoRush.Engine.River
import DinoRush.Engine.Rock

data ObstacleTag
  = ObstacleTag'GroundShort
  | ObstacleTag'GroundTall
  | ObstacleTag'Air
  | ObstacleTag'Bouncy
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Random ObstacleTag where
  randomR = randomRBoundedEnum
  random g = randomR (minBound, maxBound) g

randomRBoundedEnum :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRBoundedEnum (aMin, aMax) g = let
  (index, g') = randomR (fromEnum aMin, fromEnum aMax) g
  lastEnum = maxBound
  a = [minBound..lastEnum] !! (index `mod` fromEnum lastEnum)
  in (a, g')

streamOfObstacles :: RandomGen g => g -> [(Int, ObstacleTag)]
streamOfObstacles g = zip (map (\dist -> dist `mod` 20 + 1) $ randoms g) (randoms g)

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

stepObstacles :: Distance -> [ObstacleState] -> [ObstacleState]
stepObstacles delta = map (\o@ObstacleState{osDistance} -> ObstacleState{osDistance = osDistance - delta })

removeOutOfBoundObstacles :: [ObstacleState] -> ([ObstacleState], [ObstacleState])
removeOutOfBoundObstacles os = foldr
  (\o@ObstacleState{osDistance} (removed, remained) ->
    if inBounds osDistance
      then (o : removed, remained)
      else (removed, o : remained))
  ([], [])
  os
  where
    inBounds x = x - 32 < 0
