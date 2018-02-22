module DinoRush.Effect.HUD where

import Control.Lens (view)
import Control.Monad.State (MonadState, gets)

import DinoRush.Engine.Common
import DinoRush.Engine.Play
import DinoRush.Engine.Font
import DinoRush.Effect.Renderer

class Monad m => HUD m where
  drawHiscore :: m ()
  drawScore :: m ()

drawHiscore' :: (Renderer m, MonadState s m, HasCommonVars s) => m ()
drawHiscore' = do
  cv <- gets (view commonVars)
  drawHiscoreText (1150, 16)
  drawNumbers (fromIntegral $ cvHiscore cv) (1234, 50)

drawScore' :: (Renderer m, MonadState s m, HasPlayVars s) => m ()
drawScore' = do
  pv <- gets (view playVars)
  drawNumbers (fromIntegral $ pvScore pv) (1234, 100)

drawNumbers :: Renderer m => Integer -> (Int, Int) -> m ()
drawNumbers int (x,y) = mapM_
  (\(i, n) -> drawNumber n (x - i * 16, y))
  (zip [0..] (toNumberReverse int))
