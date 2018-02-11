module DinoRush.Engine.Rock where

import qualified Animate
import Data.Text (Text)

data RockKey
  = RockKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RockKey where
  keyName = rockKey'keyName

rockKey'keyName :: RockKey -> Text
rockKey'keyName = \case
  RockKey'Idle -> "Idle"
