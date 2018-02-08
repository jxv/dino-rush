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
import DinoRush.Scene
import DinoRush.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvLives :: Lives
  , pvSpeed :: Percent
  , pvProgress :: Distance
  , pvPlayer :: Animate.Position DinoKey Seconds
  , pvJump :: Maybe Percent
  , pvBackgroundPositionFar :: Percent
  , pvBackgroundPositionNear :: Percent
  , pvForegroundPosition :: Percent
  , pvNeargroundPosition :: Percent
  , pvObstacles :: [(Float, ObstacleTag)]
  , pvUpcomingObstacles :: [(Distance, ObstacleTag)]
  } deriving (Show, Eq)

makeClassy ''PlayVars

initPlayVars :: [(Distance, ObstacleTag)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvLives = 1
  , pvSpeed = 1
  , pvProgress = 0
  , pvJump = Nothing
  , pvPlayer = Animate.initPosition DinoKey'Move
  , pvBackgroundPositionFar = 0
  , pvBackgroundPositionNear = 0
  , pvForegroundPosition = 0
  , pvNeargroundPosition = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }

class Monad m => Play m where
  playStep :: m ()

drawPlay :: (HasPlayVars s, MonadState s m, Renderer m) => m ()
drawPlay = do
  animations <- getDinoAnimations
  pv <- gets (view playVars)
  let loc = Animate.currentLocation animations (pvPlayer pv)
  drawBackgroundFar (truncate $ 1280 * pvBackgroundPositionFar pv, backgroundFarY)
  drawBackgroundNear (truncate $ 1280 * pvBackgroundPositionNear pv, backgroundNearY)
  drawForeground (truncate $ 1280 * pvForegroundPosition pv, foregroundY)
  drawDino loc (200, dinoHeight (pvJump pv))
  drawNearground (truncate $ 1280 * pvNeargroundPosition pv, neargroundY)

playStep' :: (HasPlayVars s, MonadState s m, Logger m, Clock m, Renderer m, HasInput m, SceneManager m) => m ()
playStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  drawPlay

dinoHeight :: Maybe Percent -> Int
dinoHeight Nothing = dinoY
dinoHeight (Just (Percent percent)) = truncate (sin (percent * pi) * (-16 * 2)) + dinoY

updatePlay :: (HasPlayVars s, MonadState s m, Logger m, Clock m, Renderer m, HasInput m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  animations <- getDinoAnimations
  PlayVars{pvJump} <- gets (view playVars)
  let jump' = case pvJump of
        Just jump -> if jump >= 1 then Nothing else Just (clamp (jump + 0.06) 1)
        Nothing -> if ksStatus (iUp input) == KeyStatus'Pressed then Just 0 else Nothing
  modify $ playVars %~ (\pv -> pv
    { pvPlayer = nextFrame jump' animations (pvPlayer pv) input
    , pvJump = jump'
    , pvBackgroundPositionFar = stepHorizontal (pvBackgroundPositionFar pv) (pvSpeed pv * 0.003)
    , pvBackgroundPositionNear = stepHorizontal (pvBackgroundPositionNear pv) (pvSpeed pv * 0.006)
    , pvForegroundPosition = stepHorizontal (pvForegroundPosition pv) (pvSpeed pv * 0.009)
    , pvNeargroundPosition = stepHorizontal (pvNeargroundPosition pv) (pvSpeed pv * 0.012)
    , pvSpeed = clamp (pvSpeed pv + 0.01) 10
    })

stepHorizontal :: Percent -> Percent -> Percent
stepHorizontal percent speed = if percent' <= -1 then percent' + 1 else percent'
  where
    percent' = percent - speed

clamp :: Percent -> Percent -> Percent
clamp cur max' = if cur > max' then max' else cur

nextFrame :: Maybe Percent -> Animations DinoKey -> Animate.Position DinoKey Seconds -> Input -> Animate.Position DinoKey Seconds
nextFrame (Just 0) animations _ _ = Animate.initPositionLoops DinoKey'Kick 0
nextFrame (Just _) animations pos _ = Animate.stepPosition animations pos frameDeltaSeconds
nextFrame _ animations pos input = case ksStatus (iDown input) of
  KeyStatus'Pressed -> if Animate.pKey pos == DinoKey'Sneak then stepped else Animate.initPosition DinoKey'Sneak
  KeyStatus'Held -> if Animate.pKey pos == DinoKey'Sneak then stepped else Animate.initPosition DinoKey'Sneak
  _ -> if Animate.pKey pos == DinoKey'Move then stepped else Animate.initPosition DinoKey'Move
  where
    stepped = Animate.stepPosition animations pos frameDeltaSeconds
