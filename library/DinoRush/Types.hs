{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Types where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import Control.Lens
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import System.Random

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName DinoKey where
  keyName = dinoKey'keyName

dinoKey'keyName :: DinoKey -> Text
dinoKey'keyName = \case
  DinoKey'Idle -> "Idle"
  DinoKey'Move -> "Move"
  DinoKey'Kick -> "Kick"
  DinoKey'Hurt -> "Hurt"
  DinoKey'Sneak -> "Sneak"

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cBackgroundFar :: SDL.Texture
  , cBackgroundNear :: SDL.Texture
  , cForeground :: SDL.Texture
  , cNearground :: SDL.Texture
  , cDinoSpriteSheet :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  , cJumpSfx :: Mixer.Chunk
  }

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

data GroundShortKey
  = GroundShortKey'Idle
  deriving (Show, Eq, Ord, Enum, Bounded)

data GroundTallKey
  = GroundTallKey'Idle
  deriving (Show, Eq, Ord, Enum, Bounded)

data AirKey
  = AirKey'Idle
  deriving (Show, Eq, Ord, Enum, Bounded)

data BouncyKey
  = BouncyKey'Idle
  | BouncyKey'Jump
  deriving (Show, Eq, Ord, Enum, Bounded)

data ObstacleInfo
  = ObstacleInfo'GroundShort (Animate.Position GroundShortKey Seconds)
  | ObstacleInfo'GroundTall (Animate.Position GroundTallKey Seconds)
  | ObstacleInfo'Air (Animate.Position AirKey Seconds)
  | ObstacleInfo'Bouncy Percent (Animate.Position BouncyKey Seconds)
  deriving (Show, Eq)

data ObstacleState = ObstacleState
  { osInfo :: ObstacleInfo
  , osDistance :: Distance
  } deriving (Show, Eq)

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
