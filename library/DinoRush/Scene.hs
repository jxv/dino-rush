{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene where

import qualified Animate
import Control.Lens
import Control.Monad.State (MonadState(..), modify)

import DinoRush.GameOver
import DinoRush.Play
import DinoRush.Title
import DinoRush.Types

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Resume
  | Scene'End
  deriving (Show, Eq)

data Vars = Vars
  { vScene :: Scene
  , vDinoAnimationPosition :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

makeClassy ''Vars

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = do
  let initPlayerPosition = Animate.initPosition DinoKey'Idle
  modify $ titleVars %~ (\tv -> tv { tvPlayer = initPlayerPosition })
