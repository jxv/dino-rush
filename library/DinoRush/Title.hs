{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Title where

import DinoRush.Types

import qualified Animate

import Control.Lens
import Control.Monad.State (MonadState(..), modify)

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars initPlayerPosition
  where
    initPlayerPosition :: Animate.Position DinoKey Seconds
    initPlayerPosition = Animate.initPosition DinoKey'Idle
