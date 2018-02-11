module DinoRush.Manager.Scene
  ( SceneManager(..)
  , Scene(..)
  ) where

import DinoRush.Entity.Scene

class Monad m => SceneManager m where
  toScene :: Scene -> m ()
