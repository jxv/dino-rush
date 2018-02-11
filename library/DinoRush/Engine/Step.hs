module DinoRush.Engine.Step where

data Step a
  = Step'Change a a -- | Prev, Next
  | Step'Sustain a
  deriving (Show, Eq)

smash :: Step a -> a
smash (Step'Change _ a) = a
smash (Step'Sustain a) = a
