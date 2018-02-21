module DinoRush.Effect.Audio where

import qualified SDL.Exception as SDL
import qualified SDL.Mixer as Mixer
import Control.Monad.Reader
import Control.Exception.Safe (MonadThrow, MonadCatch, catch)

import DinoRush.Config

class Monad m => Audio m where
  playGameMusic :: m ()
  stopGameMusic :: m ()
  playJumpSfx :: m ()
  playDuckSfx :: m ()
  playPointSfx :: m ()
  playBirdSfx :: m ()
  playHurtSfx :: m ()
  playLavaSfx :: m ()
  playRockSfx :: m ()
  playQuakeSfx :: m ()
  playDeathSfx :: m ()
  playRecoverSfx :: m ()

playGameMusic' :: (MonadReader Config m, MonadIO m) => m ()
playGameMusic' = asks (rGameMusic . cResources) >>= Mixer.playMusic Mixer.Forever

stopGameMusic' :: MonadIO m => m ()
stopGameMusic' = Mixer.haltMusic

playChunk :: (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m) => (Resources -> Mixer.Chunk) -> m ()
playChunk sfx = flip catch ignore $ asks (sfx . cResources) >>= Mixer.play
  where
    ignore :: Monad m => SDL.SDLException -> m ()
    ignore _ = return ()

playJumpSfx',
  playDuckSfx',
  playPointSfx',
  playBirdSfx',
  playHurtSfx',
  playLavaSfx',
  playRockSfx',
  playQuakeSfx',
  playDeathSfx',
  playRecoverSfx' :: (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m) => m ()
playJumpSfx' = playChunk rJumpSfx
playDuckSfx' = playChunk rDuckSfx
playPointSfx' = playChunk rPointSfx
playBirdSfx' = playChunk rBirdSfx
playHurtSfx' = playChunk rHurtSfx
playLavaSfx' = playChunk rLavaSfx
playRockSfx' = playChunk rRockSfx
playQuakeSfx' = playChunk rQuakeSfx
playDeathSfx' = playChunk rDeathSfx
playRecoverSfx' = playChunk rRecoverSfx
