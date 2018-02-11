{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.Title where

import qualified Animate
import Control.Lens

import DinoRush.Engine.Types
import DinoRush.Engine.Dino
import DinoRush.Engine.Mountain

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  , tvMountainPos :: Animate.Position MountainKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle) (Animate.initPosition MountainKey'Idle)
