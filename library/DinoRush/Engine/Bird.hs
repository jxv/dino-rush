module DinoRush.Engine.Bird where

import Data.Text (Text)
import qualified Animate

data BirdKey
  = BirdKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BirdKey where
  keyName = birdKey'keyName

birdKey'keyName :: BirdKey -> Text
birdKey'keyName = \case
  BirdKey'Idle -> "Idle"
