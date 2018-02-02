{-# LANGUAGE TemplateHaskell #-}
module DinoRush.GameOver where

import qualified Animate
import Control.Lens

import DinoRush.Types

data EndVars = EndVars
  { evPlayer :: Animate.Position DinoKey Seconds
  , evBackgroundPositionFar :: Percent
  , evBackgroundPositionClose :: Percent
  , evForegroundPosition :: Percent
  , evObstacles :: [(Distance, Obstacle)]
  } deriving (Show, Eq)

makeClassy ''EndVars
