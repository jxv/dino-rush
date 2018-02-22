{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.Common where

import Control.Lens
import DinoRush.Engine.Types

data CommonVars = CommonVars
  { cvHiscore :: Score
  } deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars 0
