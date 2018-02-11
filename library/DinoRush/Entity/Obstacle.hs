module DinoRush.Entity.Obstacle where

import qualified Animate

import System.Random

import DinoRush.Engine.Types
import DinoRush.Entity.Bird
import DinoRush.Entity.Bouncer
import DinoRush.Entity.Ground
import DinoRush.Entity.Jungle
import DinoRush.Entity.Lava
import DinoRush.Entity.Mountain
import DinoRush.Entity.River
import DinoRush.Entity.Rock

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

streamOfObstacles :: RandomGen g => g -> [(Distance, ObstacleTag)]
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

stepObstacles :: Float -> [(Float, ObstacleTag)] -> [(Float, ObstacleTag)]
stepObstacles delta = map (\(loc, obs) -> (loc - delta, obs))
