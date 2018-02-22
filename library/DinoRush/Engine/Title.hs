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
  , tvFlashing :: Float
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle) (Animate.initPosition MountainKey'Idle) 0

titleShowPressSpace :: Float -> Bool
titleShowPressSpace p = sin p > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5
