module DinoRush.Engine.Types where

import qualified Animate

import Data.Aeson (FromJSON, ToJSON)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

newtype Stocks = Stocks Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Percent = Percent Float
  deriving (Show, Eq, Num, Fractional, Floating, RealFrac, Real, Ord)

newtype Distance = Distance Float
  deriving (Show, Eq, Num, Fractional, RealFrac, Real, Ord)

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

newtype Score = Score Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

clamp :: (Fractional a, Ord a) => a -> a -> a -> a
clamp cur min' max' = if cur > max' then max' else (if cur < min' then min' else cur)
