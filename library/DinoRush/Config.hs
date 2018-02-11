module DinoRush.Config where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import DinoRush.Entity.Bird
import DinoRush.Entity.Bouncer
import DinoRush.Entity.Dino
import DinoRush.Entity.Mountain
import DinoRush.Entity.Lava
import DinoRush.Entity.Rock
import DinoRush.Engine.Types

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }

data Resources = Resources
  { rMountainSprites :: Animate.SpriteSheet MountainKey SDL.Texture Seconds
  , rBackgroundNear :: SDL.Texture
  , rForeground :: SDL.Texture
  , rNearground :: SDL.Texture
  , rDinoSprites :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  , rBirdSprites :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  , rBouncerSprites :: Animate.SpriteSheet BouncerKey SDL.Texture Seconds
  , rLavaSprites :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  , rRockSprites :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , rGameMusic :: Mixer.Music
  , rJumpSfx :: Mixer.Chunk
  }
