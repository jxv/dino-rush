module DinoRush.Engine.Obstacle where

import qualified Animate
import System.Random
import Linear (V2(..))

import DinoRush.Engine.Types
import DinoRush.Engine.Bird
import DinoRush.Engine.Lava
import DinoRush.Engine.Rock
import DinoRush.Engine.Physics

data ObstacleTag
  = ObstacleTag'Lava
  | ObstacleTag'Rock
  | ObstacleTag'Bird
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Random ObstacleTag where
  randomR = randomRBoundedEnum
  random g = randomR (minBound, maxBound) g

data ObstacleInfo
  = ObstacleInfo'Lava (Animate.Position LavaKey Seconds)
  | ObstacleInfo'Rock (Animate.Position RockKey Seconds)
  | ObstacleInfo'Bird (Animate.Position BirdKey Seconds)
  deriving (Show, Eq)

data ObstacleState = ObstacleState
  { osInfo :: ObstacleInfo
  , osDistance :: Distance
  } deriving (Show, Eq)

lavaY, rockY, birdY :: Num a => a
lavaY = 16 * 28
rockY = 16 * 26 + 2
birdY = 16 * 22

obstacleAabb :: ObstacleState -> Aabb
obstacleAabb ObstacleState{osInfo,osDistance} = case osInfo of
  ObstacleInfo'Lava _ -> Aabb (V2 (0 + dist) lavaY) (V2 (32 + dist) (lavaY + 32))
  ObstacleInfo'Rock _ -> Aabb (V2 (0 + dist) rockY) (V2 (32 + dist) (rockY + 32))
  ObstacleInfo'Bird _ -> Aabb (V2 (0 + dist) birdY) (V2 (32 + dist) (birdY + 32))
  where
    dist = realToFrac osDistance

randomRBoundedEnum :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRBoundedEnum (aMin, aMax) g = let
  (index, g') = randomR (fromEnum aMin, fromEnum aMax) g
  lastEnum = maxBound
  a = [minBound..lastEnum] !! (index `mod` (fromEnum lastEnum + 1))
  in (a, g')

streamOfObstacles :: RandomGen g => g -> [(Int, ObstacleTag)]
streamOfObstacles g = zip (map (\dist -> dist `mod` 18 + 3) $ randoms g) (randoms g)

stepObstacles :: Distance -> [ObstacleState] -> [ObstacleState]
stepObstacles delta = map
  (\o@ObstacleState{osInfo, osDistance} -> o { osDistance = osDistance - (case osInfo of ObstacleInfo'Bird _ -> delta + 3; _ -> delta) })

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
  , osDistance = realToFrac (idxDist * 32) + realToFrac arenaWidth
  }

lastObstacleDistance :: [ObstacleState] -> Distance
lastObstacleDistance os = maximum $ (realToFrac arenaWidth - 1) : map osDistance os

canAddObstacle :: Distance -> Bool
canAddObstacle dist = dist < realToFrac arenaWidth

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
