{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Title where

import qualified SDL
import qualified Animate
import Foreign.C.Types

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Linear

import DinoRush.Clock
import DinoRush.Logger
import DinoRush.Input
import DinoRush.Renderer
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

detectSpacePressed :: SDL.EventPayload -> Bool
detectSpacePressed event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == SDL.KeycodeSpace &&
    motion == SDL.Pressed &&
    not keyboardEventRepeat
  _ -> False

rectFromClip :: Animate.SpriteClip -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

titleStep :: (HasTitleVars s, MonadReader Config m, MonadState s m, Logger m, Clock m, Renderer m, Input m) => m ()
titleStep = do
  events <- getEventPayloads
  Animate.SpriteSheet{ssAnimations, ssImage} <- asks cDinoSpriteSheet
  pos <- gets (tvPlayer . view titleVars)
  let toNextKey = any detectSpacePressed events
  let pos' = Animate.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Animate.currentLocation ssAnimations pos'
  drawSurfaceToScreen ssImage (Just $ rectFromClip loc) (Just $ SDL.P $ V2 80 60)
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ logText $ Animate.keyName (Animate.pKey pos'')
  modify $ titleVars %~ (\tv -> tv { tvPlayer = pos'' })
