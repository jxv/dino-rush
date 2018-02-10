{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.Types where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import Control.Lens
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import System.Random

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

data Step a
  = Step'Change a a -- | Prev, Next
  | Step'Sustain a
  deriving (Show, Eq)

smash :: Step a -> a
smash (Step'Change _ a) = a
smash (Step'Sustain a) = a

newtype Lives = Lives Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Percent = Percent Float
  deriving (Show, Eq, Num, Fractional, RealFrac, Real, Ord)

newtype Distance = Distance Integer
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum, Random)

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

newtype Score = Score Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

frameDeltaSeconds :: Fractional a => a
frameDeltaSeconds = 0.016667

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 16

clamp :: Percent -> Percent -> Percent
clamp cur max' = if cur > max' then max' else cur

--

data MountainKey
  = MountainKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName MountainKey where
  keyName = mountainKey'keyName

mountainKey'keyName :: MountainKey -> Text
mountainKey'keyName = \case
  MountainKey'Idle -> "Idle"

data BirdKey
  = BirdKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BirdKey where
  keyName = birdKey'keyName

birdKey'keyName :: BirdKey -> Text
birdKey'keyName = \case
  BirdKey'Idle -> "Idle"

data BouncerKey
  = BouncerKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BouncerKey where
  keyName = bouncerKey'keyName

bouncerKey'keyName :: BouncerKey -> Text
bouncerKey'keyName = \case
  BouncerKey'Idle -> "Idle"

data LavaKey
  = LavaKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName LavaKey where
  keyName = lavaKey'keyName

lavaKey'keyName :: LavaKey -> Text
lavaKey'keyName = \case
  LavaKey'Idle -> "Idle"

data RockKey
  = RockKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RockKey where
  keyName = rockKey'keyName

rockKey'keyName :: RockKey -> Text
rockKey'keyName = \case
  RockKey'Idle -> "Idle"

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
