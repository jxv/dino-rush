{-# LANGUAGE TemplateHaskell #-}
module DinoRush.State where

import Control.Lens

import DinoRush.Engine.Common
import DinoRush.Engine.Scene
import DinoRush.Engine.Input
import DinoRush.Engine.Camera
import DinoRush.Engine.Obstacle
import DinoRush.Engine.Play
import DinoRush.Engine.GameOver
import DinoRush.Engine.Title

data Vars = Vars
  { vCommon :: CommonVars
  , vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vGameOver :: GameOverVars
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: [(Int, ObstacleTag)] -> Vars
initVars mkObstacles = Vars initCommonVars Scene'Title Scene'Title initTitleVars (initPlayVars mkObstacles) initGameOverVars initInput initCamera

instance HasCommonVars Vars where
  commonVars = lens vCommon (\v s -> v { vCommon = s })

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Vars where
  playVars = lens vPlay (\v s -> v { vPlay = s })

instance HasGameOverVars Vars where
  gameOverVars = lens vGameOver (\v s -> v { vGameOver = s })

makeClassy ''Vars
