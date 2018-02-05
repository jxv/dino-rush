{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Play where

import qualified SDL
import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Foreign.C.Types
import Linear
import KeyState

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Input
import DinoRush.Renderer
import DinoRush.Sprite
import DinoRush.Types

import DinoRush.SDL.Renderer

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

initPlayVars :: [(Distance, Obstacle)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvLives = 1
  , pvProgress = 0
  , pvPlayer = Animate.initPosition DinoKey'Idle
  , pvBackgroundPositionFar = 0
  , pvBackgroundPositionClose = 0
  , pvForegroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }

class Monad m => Play m where
  playStep :: m ()

playStep' :: (HasPlayVars s, MonadReader Config m, MonadState s m, Logger m, Clock m, Renderer m, HasInput m, SpriteManager m) => m ()
playStep' = do
  input <- getInput
  animations <- getDinoAnimations
  pos <- gets (pvPlayer . view playVars)
  let pos' = Animate.stepPosition animations pos frameDeltaSeconds
  let loc = Animate.currentLocation animations pos'
  drawDino loc (80, 60)
  let toNextKey = ksStatus (iSpace input) == KeyStatus'Pressed
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ logText $ Animate.keyName (Animate.pKey pos'')
  modify $ playVars %~ (\pv -> pv { pvPlayer = pos'' })
