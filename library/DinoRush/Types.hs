{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Types where

import qualified SDL
import qualified Animate

import Control.Lens
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.Key DinoKey
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
  , cScreen :: SDL.Surface
  , cDinoSpriteSheet :: Animate.SpriteSheet DinoKey SDL.Surface Seconds
  }

data Obstacle
  = Obstacle'GroundShort
  | Obstacle'GroundTall
  | Obstacle'Air
  | Obstacle'Bouncy Percent
  deriving (Show, Eq)

newtype Lives = Lives Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Percent = Percent Float
  deriving (Show, Eq, Num, Fractional, Ord)

newtype Distance = Distance Float
  deriving (Show, Eq, Num)

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

newtype Score = Score Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)
