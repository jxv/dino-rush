module DinoRush.Audio where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader

import DinoRush.Types

class Monad m => Audio m where
  playJumpSfx :: m ()

playJumpSfx' :: (MonadReader Config m, MonadIO m) => m ()
playJumpSfx' = do
  sfx <- asks cJumpSfx
  Mixer.play sfx
