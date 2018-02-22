{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import DinoRush.Config
import DinoRush.Effect.Audio
import DinoRush.Effect.Camera
import DinoRush.Effect.Clock
import DinoRush.Effect.Logger
import DinoRush.Effect.Renderer
import DinoRush.Engine.Camera
import DinoRush.Engine.Input
import DinoRush.Engine.Frame
import DinoRush.Engine.Play
import DinoRush.Engine.Title
import DinoRush.Manager.Input
import DinoRush.Manager.Scene
import DinoRush.Scene.GameOver
import DinoRush.Scene.Play
import DinoRush.Scene.Death
import DinoRush.Scene.Pause
import DinoRush.Scene.Title

import DinoRush.State

titleTransition :: (HasTitleVars a, MonadState a m, CameraControl m) => m ()
titleTransition = do
  adjustCamera initCamera
  modify $ titleVars .~ initTitleVars

playTransition :: (HasPlayVars a, MonadState a m, Audio m) => m ()
playTransition = do
  PlayVars{pvUpcomingObstacles,pvHiscore} <- gets (view playVars)
  modify $ playVars .~ (initPlayVars pvUpcomingObstacles pvHiscore)
  playGameMusic

deathTransition :: (Audio m) => m ()
deathTransition = do
  stopGameMusic
  playDeathSfx

pauseToPlay :: Audio m => m ()
pauseToPlay = raiseGameMusic

playToPause :: Audio m => m ()
playToPause = lowerGameMusic

toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

mainLoop ::
  ( MonadReader Config m
  , MonadState Vars m
  , Audio m
  , Logger m
  , Clock m
  , CameraControl m
  , Renderer m
  , HasInput m
  , Title m
  , Play m
  , Pause m
  , Death m
  , GameOver m
  ) => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  scene <- gets vScene
  step scene
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  stepScene scene nextScene
  let quit = nextScene == Scene'Quit || iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop
  where

    step scene = do
      case scene of
        Scene'Title -> titleStep
        Scene'Play -> playStep
        Scene'Pause -> pauseStep
        Scene'Death -> deathStep
        Scene'GameOver -> gameOverStep
        Scene'Quit -> return ()

    stepScene scene nextScene = do
      when (nextScene /= scene) $ do
        case nextScene of
          Scene'Title -> titleTransition
          Scene'Play -> case scene of
            Scene'Title -> playTransition
            Scene'Pause -> pauseToPlay
            _ -> return ()
          Scene'Death -> case scene of
            Scene'Play -> deathTransition
            _ -> return ()
          Scene'Pause -> case scene of
            Scene'Play -> playToPause
            _ -> return ()
          Scene'GameOver -> return ()
          Scene'Quit -> return ()
        modify (\v -> v { vScene = nextScene })
