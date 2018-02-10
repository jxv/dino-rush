module DinoRush.Effect.Audio where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader

import DinoRush.Config

class Monad m => Audio m where
  playGameMusic :: m ()
  playJumpSfx :: m ()

playGameMusic' :: (MonadReader Config m, MonadIO m) => m ()
playGameMusic' = asks cGameMusic >>= Mixer.playMusic Mixer.Forever

playJumpSfx' :: (MonadReader Config m, MonadIO m) => m ()
playJumpSfx' = asks cJumpSfx >>= Mixer.play
