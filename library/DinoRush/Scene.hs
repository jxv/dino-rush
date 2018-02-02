{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene where

import Control.Lens
import Control.Monad.State (MonadState(..), modify)

import DinoRush.GameOver
import DinoRush.Play
import DinoRush.Title (initTitleVars, HasTitleVars(..), TitleVars(..))
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

makeClassy ''Vars

titleTransition :: (HasTitleVars a, MonadState a m) => m ()
titleTransition = modify $ titleVars .~ initTitleVars
