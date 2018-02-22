module DinoRush.Engine.Quake where

import DinoRush.Engine.Types
import DinoRush.Engine.Frame
import DinoRush.Engine.Step

data Quake
  = Quake'Progress Percent
  | Quake'Dormant Seconds
  deriving (Show, Eq)

stepQuake :: Quake -> Step Quake
stepQuake q@(Quake'Progress p)
  | p' >= 1 = Step'Change q (Quake'Dormant 30)
  | otherwise = Step'Sustain (Quake'Progress p')
  where
    p' = p + 0.01
stepQuake q@(Quake'Dormant s)
  | s' <= 0 = Step'Change q (Quake'Progress 0)
  | otherwise = Step'Sustain (Quake'Dormant s')
  where
    s' = s - frameDeltaSeconds

startQuake :: Step Quake -> Bool
startQuake (Step'Change _ (Quake'Progress _)) = True
startQuake _ = False
