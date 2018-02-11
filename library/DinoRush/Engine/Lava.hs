module DinoRush.Engine.Lava where

import qualified Animate
import Data.Text (Text)

data LavaKey
  = LavaKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName LavaKey where
  keyName = lavaKey'keyName

lavaKey'keyName :: LavaKey -> Text
lavaKey'keyName = \case
  LavaKey'Idle -> "Idle"
