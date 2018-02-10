module DinoRush.Dino where

import qualified Safe
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import Control.Lens
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import System.Random
import KeyState

import DinoRush.Types
import DinoRush.Input
import DinoRush.Physics

data DinoAction
  = DinoAction'Move
  | DinoAction'Duck
  | DinoAction'Jump Percent
  deriving (Show, Eq)

data DinoSfx
  = DinoSfx'Jump
  deriving (Show, Eq)

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName DinoKey where
  keyName = dinoKey'keyName

dinoKey'keyName :: DinoKey -> Text
dinoKey'keyName = \case
  DinoKey'Idle -> "Idle"
  DinoKey'Move -> "Move"
  DinoKey'Kick -> "Kick"
  DinoKey'Hurt -> "Hurt"
  DinoKey'Sneak -> "Sneak"

dinoX :: Float
dinoX = 200

dinoY :: Int
dinoY = 16 * 26

rightEdge :: Float
rightEdge = arenaWidth - (dinoX + 48)

dinoHeight :: DinoAction -> Int
dinoHeight (DinoAction'Jump (Percent percent)) = truncate (sin (percent * pi) * (-16 * 2)) + dinoY
dinoHeight _ = dinoY

distanceFromLastObstacle :: [(Float, ObstacleTag)] -> Float
distanceFromLastObstacle obstacles = case Safe.lastMay obstacles of
  Nothing -> rightEdge
  Just (dist, _) -> rightEdge - dist

stepDinoAction :: Input -> DinoAction -> Step DinoAction
stepDinoAction input da = case da of
  DinoAction'Move -> case ksStatus (iDown input) of
    KeyStatus'Pressed -> Step'Change da DinoAction'Duck
    _ -> case ksStatus (iUp input) of
      KeyStatus'Pressed -> Step'Change da $ DinoAction'Jump 0
      _ -> Step'Sustain DinoAction'Move
  DinoAction'Duck -> case ksStatus (iDown input) of
    KeyStatus'Held -> Step'Sustain DinoAction'Duck
    _ -> Step'Change da DinoAction'Move
  DinoAction'Jump percent -> if percent >= 1
    then Step'Change da DinoAction'Move
    else Step'Sustain $ DinoAction'Jump (clamp (percent + 0.06) 1)

stepDinoPosition :: Step DinoAction -> Animations DinoKey -> Animate.Position DinoKey Seconds -> Animate.Position DinoKey Seconds
stepDinoPosition (Step'Sustain _) animations pos = Animate.stepPosition animations pos frameDeltaSeconds
stepDinoPosition (Step'Change _ da) _ _ = case da of
  DinoAction'Move -> Animate.initPosition DinoKey'Move
  DinoAction'Duck -> Animate.initPosition DinoKey'Sneak
  DinoAction'Jump _ -> Animate.initPositionLoops DinoKey'Kick 0

stepDinoSfx :: Step DinoAction -> [DinoSfx]
stepDinoSfx (Step'Sustain _) = []
stepDinoSfx (Step'Change _ da) = case da of
  DinoAction'Jump _ -> [DinoSfx'Jump]
  _ -> []
