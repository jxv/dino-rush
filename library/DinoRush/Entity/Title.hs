{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Entity.Title where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import DinoRush.Config
import DinoRush.Effect.Renderer
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Types
import DinoRush.Entity.Dino
import DinoRush.Entity.Mountain
import DinoRush.Entity.Obstacle

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  , tvMountainPos :: Animate.Position MountainKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle) (Animate.initPosition MountainKey'Idle)
