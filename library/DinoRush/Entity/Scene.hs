module DinoRush.Entity.Scene where

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)
