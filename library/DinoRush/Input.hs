module DinoRush.Input where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import KeyState

import DinoRush.Types

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState initKeyState initKeyState

keycodePressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodePressed keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Pressed &&
    not keyboardEventRepeat
  _ -> False

class Monad m => SDLInput m where
  pollEventPayloads :: m [SDL.EventPayload]

class Monad m => HasInput m where
  setInput :: Input -> m ()
  getInput :: m Input

pollEventPayloads' :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events input = Input
  { iSpace = updateKeyState 1 (iSpace input) (pressed SDL.KeycodeSpace)
  , iUp = updateKeyState 1 (iUp input) (pressed SDL.KeycodeUp)
  , iDown = updateKeyState 1 (iDown input) (pressed SDL.KeycodeDown)
  , iEscape = updateKeyState 1 (iEscape input) (pressed SDL.KeycodeEscape)
  }
  where
    pressed keycode = or $ map (keycodePressed keycode) events
