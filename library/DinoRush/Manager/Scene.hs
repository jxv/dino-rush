{-# LANGUAGE TemplateHaskell #-}
module DinoRush.Manager.Scene where

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

class Monad m => SceneManager m where
  toScene :: Scene -> m ()
