{-# LANGUAGE TemplateHaskell #-}
module DinoRush.GameOver where

import qualified Animate
import Control.Lens

import DinoRush.Types

data GameOverVars = GameOverVars
  { govPlayer :: Animate.Position DinoKey Seconds
  , govBackgroundPositionFar :: Percent
  , govBackgroundPositionClose :: Percent
  , govForegroundPosition :: Percent
  , govObstacles :: [(Distance, Obstacle)]
  } deriving (Show, Eq)

makeClassy ''GameOverVars

gameOverStep :: Monad m => m ()
gameOverStep = return ()
