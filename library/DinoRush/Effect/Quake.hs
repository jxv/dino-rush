module DinoRush.Effect.Quake where

import Control.Lens
import Control.Monad.State (MonadState(..), modify, gets)

import DinoRush.Effect.Sfx
import DinoRush.Engine.Common
import DinoRush.Engine.Quake
import DinoRush.Engine.Step
import DinoRush.Engine.Sfx

updateQuake :: (AudioSfx m, MonadState s m, HasCommonVars s) => m ()
updateQuake = do
  sq <- stepQuake <$> gets (cvQuake . view commonVars)
  addSfxs $ if startQuake sq then [Sfx'Quake] else []
  modify $ commonVars %~ (\cv -> cv{ cvQuake = smash sq })
