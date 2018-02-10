module DinoRush.Config where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import DinoRush.Entity.Dino
import DinoRush.Engine.Types

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cMountainSprites :: Animate.SpriteSheet MountainKey SDL.Texture Seconds
  , cBackgroundNear :: SDL.Texture
  , cForeground :: SDL.Texture
  , cNearground :: SDL.Texture
  , cDinoSpriteSheet :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  , cBirdSpriteSheet :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  , cBouncerSpriteSheet :: Animate.SpriteSheet BouncerKey SDL.Texture Seconds
  , cLavaSpriteSheet :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  , cRockSpriteSheet :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , cJumpSfx :: Mixer.Chunk
  , cGameMusic :: Mixer.Music
  }
