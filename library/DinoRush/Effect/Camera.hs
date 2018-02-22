module DinoRush.Effect.Camera where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, modify, gets)
import Data.StateVar (($=))
import SDL.Vect

import DinoRush.Config
import DinoRush.State
import DinoRush.Engine.Camera

class Monad m => CameraControl m where
  adjustCamera :: Camera -> m ()
  disableZoom :: m ()
  enableZoom :: m ()

adjustCamera' :: (MonadIO m, MonadReader Config m, MonadState Vars m) => Camera -> m ()
adjustCamera' cam = do
  modify $ \v -> v { vCamera = cam }
  renderer <- asks cRenderer
  moveCamera renderer cam

disableZoom' :: (MonadIO m, MonadReader Config m) => m ()
disableZoom' = do
  renderer <- asks cRenderer
  moveCamera renderer initCamera

enableZoom' :: (MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
enableZoom' = do
  renderer <- asks cRenderer
  cam <- gets vCamera
  moveCamera renderer cam

moveCamera :: MonadIO m => SDL.Renderer -> Camera -> m ()
moveCamera renderer Camera{camZoom, camOrigin} = do
  SDL.rendererScale renderer $= (fmap realToFrac camZoom)
  let dim = fmap truncate $ screenV2
  SDL.rendererViewport renderer $= (Just $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim)
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)
