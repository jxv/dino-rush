{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Play where

import Control.Lens
import qualified Animate

import DinoRush.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvLives :: Lives
  , pvProgress :: Percent
  , pvPlayer :: Animate.Position DinoKey Seconds
  , pvBackgroundPositionFar :: Percent
  , pvBackgroundPositionClose :: Percent
  , pvForegroundPosition :: Percent
  , pvObstacles :: [(Distance, Obstacle)]
  , pvUpcomingObstacles :: [(Distance, Obstacle)]
  } deriving (Show, Eq)

makeClassy ''PlayVars

playStep :: Monad m => m ()
playStep = return ()
