module DinoRush.Entity.Bouncer where

import Data.Text (Text)
import qualified Animate

data BouncerKey
  = BouncerKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BouncerKey where
  keyName = bouncerKey'keyName

bouncerKey'keyName :: BouncerKey -> Text
bouncerKey'keyName = \case
  BouncerKey'Idle -> "Idle"
