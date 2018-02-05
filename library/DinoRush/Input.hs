module DinoRush.Input where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import KeyState

import DinoRush.SDL.Input
import DinoRush.Types

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState initKeyState initKeyState False

class Monad m => HasInput m where
  updateInput :: m ()
  setInput :: Input -> m ()
  getInput :: m Input

updateInput' :: (HasInput m, SDLInput m) => m ()
updateInput' = do
  input <- getInput
  events <- pollEventPayloads
  setInput (stepControl events input)

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events input = Input
  { iSpace = updateKeyState 1 (iSpace input) (pressed SDL.KeycodeSpace)
  , iUp = updateKeyState 1 (iUp input) (pressed SDL.KeycodeUp)
  , iDown = updateKeyState 1 (iDown input) (pressed SDL.KeycodeDown)
  , iEscape = updateKeyState 1 (iEscape input) (pressed SDL.KeycodeEscape)
  , iQuit = elem SDL.QuitEvent events
  }
  where
    pressed keycode = or $ map (keycodePressed keycode) events
