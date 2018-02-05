{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Play where

import qualified Animate
import Control.Monad (when)
import Control.Lens
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Input
import DinoRush.Renderer
import DinoRush.Sprite
import DinoRush.Scene
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

initPlayVars :: [(Distance, Obstacle)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvLives = 1
  , pvProgress = 0
  , pvPlayer = Animate.initPosition DinoKey'Move
  , pvBackgroundPositionFar = 0
  , pvBackgroundPositionClose = 0
  , pvForegroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }

class Monad m => Play m where
  playStep :: m ()

playStep' :: (HasPlayVars s, MonadState s m, Logger m, Clock m, Renderer m, HasInput m, SpriteManager m, SceneManager m) => m ()
playStep' = do
  input <- getInput
  animations <- getDinoAnimations
  pos <- gets (pvPlayer . view playVars)
  let pos' = nextFrame animations pos input
  let loc = Animate.currentLocation animations pos'
  drawDino loc (200, 400)
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  modify $ playVars %~ (\pv -> pv { pvPlayer = pos' })

nextFrame :: Animations DinoKey -> Animate.Position DinoKey Seconds -> Input -> Animate.Position DinoKey Seconds
nextFrame animations pos input = case ksStatus (iDown input) of
  KeyStatus'Pressed -> if Animate.pKey pos == DinoKey'Sneak then stepped else Animate.initPosition DinoKey'Sneak
  KeyStatus'Held -> if Animate.pKey pos == DinoKey'Sneak then stepped else Animate.initPosition DinoKey'Sneak
  _ -> if Animate.pKey pos == DinoKey'Move then stepped else Animate.initPosition DinoKey'Move
  where
    stepped = Animate.stepPosition animations pos frameDeltaSeconds
