{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.GameOver where

import qualified Animate
import Control.Lens

import DinoRush.Types

data GameOverVars = GameOverVars
  { govPlayer :: Animate.Position DinoKey Seconds
  , govMountainScroll :: Percent
  , govBackgroundPositionClose :: Percent
  , govForegroundPosition :: Percent
  , govObstacles :: [(Distance, ObstacleTag)]
  } deriving (Show, Eq)

makeClassy ''GameOverVars

gameOverStep :: Monad m => m ()
gameOverStep = return ()
