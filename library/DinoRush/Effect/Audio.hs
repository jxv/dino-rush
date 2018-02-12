module DinoRush.Effect.Audio where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader

import DinoRush.Config

class Monad m => Audio m where
  playGameMusic :: m ()
  playJumpSfx :: m ()
  playPointSfx :: m ()

playGameMusic' :: (MonadReader Config m, MonadIO m) => m ()
playGameMusic' = asks (rGameMusic . cResources) >>= Mixer.playMusic Mixer.Forever

playJumpSfx' :: (MonadReader Config m, MonadIO m) => m ()
playJumpSfx' = asks (rJumpSfx . cResources) >>= Mixer.play

playPointSfx' :: (MonadReader Config m, MonadIO m) => m ()
playPointSfx' = asks (rPointSfx . cResources) >>= Mixer.play
