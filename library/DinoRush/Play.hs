{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Play where

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
  , pvPlayer = Animate.initPosition DinoKey'Idle
  , pvBackgroundPositionFar = 0
  , pvBackgroundPositionClose = 0
  , pvForegroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }

playStep :: (HasPlayVars s, MonadReader Config m, MonadState s m, Logger m, Clock m, Renderer m, HasInput m) => m ()
playStep = do
  input <- getInput

  Animate.SpriteSheet{ssAnimations, ssImage} <- asks cDinoSpriteSheet
  pos <- gets (pvPlayer . view playVars)
  let pos' = Animate.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Animate.currentLocation ssAnimations pos'
  drawSurfaceToScreen ssImage (Just $ rectFromClip loc) (Just $ SDL.P $ V2 80 60)

  let toNextKey = ksStatus (iSpace input) == KeyStatus'Pressed
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ logText $ Animate.keyName (Animate.pKey pos'')
  modify $ playVars %~ (\pv -> pv { pvPlayer = pos'' })
