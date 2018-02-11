{-# LANGUAGE TemplateHaskell #-}
module DinoRush.State where

import Control.Lens

import DinoRush.Manager.Scene
import DinoRush.Engine.Input
import DinoRush.Engine.Types
import DinoRush.Entity.Obstacle
import DinoRush.Entity.Play
import DinoRush.Entity.Title

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vInput :: Input
  } deriving (Show, Eq)

initVars :: [(Distance, ObstacleTag)] -> Vars
initVars mkObstacles = Vars Scene'Title Scene'Title initTitleVars (initPlayVars mkObstacles) initInput

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Vars where
  playVars = lens vPlay (\v s -> v { vPlay = s })

makeClassy ''Vars
