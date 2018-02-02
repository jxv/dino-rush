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

f :: (HasTitleVars a, MonadState a m) => m ()
f = do
  let initPlayerPosition = Animate.initPosition DinoKey'Idle
  modify $ titleVars %~ (\tv -> tv { tvPlayer = initPlayerPosition })
