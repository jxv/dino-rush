{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene where

import Control.Lens
import Control.Monad.State (MonadState(..), modify)
import Control.Monad.Reader (MonadReader(..))

import DinoRush.GameOver
import DinoRush.Clock
import DinoRush.Play
import DinoRush.Logger
import DinoRush.Renderer
import DinoRush.Input
import DinoRush.Title (titleLoop, initTitleVars, TitleVars(..), HasTitleVars(..))
import DinoRush.Types

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Resume
  | Scene'End
  deriving (Show, Eq)

data Vars = Vars
  { vScene :: Scene
  , vTitle :: TitleVars
  } deriving (Show, Eq)

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v t -> v { vTitle = t })

makeClassy ''Vars

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = modify $ titleVars .~ initTitleVars

sceneLoop :: (MonadReader Config m, MonadState Vars m, Logger m, Clock m, Renderer m, Input m) => m ()
sceneLoop = titleLoop
