{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Engine.GameOver where

import Control.Lens

import DinoRush.Engine.Types

data GameOverVars = GameOverVars
  { govFadeout :: Percent
  , govSpaceFlashing :: Percent
  } deriving (Show, Eq)

makeClassy ''GameOverVars

initGameOverVars :: GameOverVars
initGameOverVars = GameOverVars 0 0

stepGameOverVars :: GameOverVars -> GameOverVars
stepGameOverVars gov = gov
  { govFadeout = clamp (govFadeout gov + 0.01) 0 1
  , govSpaceFlashing = govSpaceFlashing gov + 0.05
  }

gameOverShowPressSpace :: Percent -> Bool
gameOverShowPressSpace p = sin p < 0
