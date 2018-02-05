{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Scene.Title where

import qualified SDL
import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Linear
import KeyState

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Input
import DinoRush.Renderer
import DinoRush.Scene
import DinoRush.Types
import DinoRush.Sprite

import DinoRush.SDL.Renderer

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars initPlayerPosition
  where
    initPlayerPosition :: Animate.Position DinoKey Seconds
    initPlayerPosition = Animate.initPosition DinoKey'Idle

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (SceneManager m, HasTitleVars s, MonadReader Config m, MonadState s m, Logger m, Clock m, SDLRenderer m, HasInput m, SpriteManager m) => m ()
titleStep' = do
  input <- getInput
  animations <- getDinoAnimations
  pos <- gets (tvPlayer . view titleVars)
  let pos' = Animate.stepPosition animations pos frameDeltaSeconds
  let loc = Animate.currentLocation animations pos'
  drawDino loc (200, 400)
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos' })
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
