{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Title where

import qualified SDL
import qualified Animate
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Foreign.C.Types
import Linear
import KeyState

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Input
import DinoRush.Renderer
import DinoRush.Scene
import DinoRush.Types

data TitleVars = TitleVars
  { tvPlayer :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars initPlayerPosition
  where
    initPlayerPosition :: Animate.Position DinoKey Seconds
    initPlayerPosition = Animate.initPosition DinoKey'Idle

titleStep :: (SceneManager m, HasTitleVars s, MonadReader Config m, MonadState s m, Logger m, Clock m, Renderer m, HasInput m) => m ()
titleStep = do
  input <- getInput
  Animate.SpriteSheet{ssAnimations, ssImage} <- asks cDinoSpriteSheet
  pos <- gets (tvPlayer . view titleVars)
  let pos' = Animate.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Animate.currentLocation ssAnimations pos'
  drawSurfaceToScreen ssImage (Just $ rectFromClip loc) (Just $ SDL.P $ V2 80 60)
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos' })
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
