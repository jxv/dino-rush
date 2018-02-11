module DinoRush.Engine.Input where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import KeyState

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState initKeyState initKeyState False
