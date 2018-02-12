module DinoRush.Effect.Camera where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Data.StateVar (($=))
import SDL.Vect

import DinoRush.Config
import DinoRush.Engine.Camera

class Monad m => CameraControl m where
  adjustCamera :: Camera -> m ()

adjustCamera' :: (MonadIO m, MonadReader Config m) => Camera -> m ()
adjustCamera' Camera{camZoom, camOrigin} = do
  renderer <- asks cRenderer
  SDL.rendererScale renderer $= (fmap realToFrac camZoom)
  let dim = fmap truncate $ screenV2
  SDL.rendererViewport renderer $= (Just $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim)
  -- SDL.rendererViewport renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)
