module DinoRush.Engine.River where

import Data.Text (Text)
import qualified Animate

data RiverKey
  = RiverKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RiverKey where
  keyName = mountainKey'keyName

mountainKey'keyName :: RiverKey -> Text
mountainKey'keyName = \case
  RiverKey'Idle -> "Idle"
