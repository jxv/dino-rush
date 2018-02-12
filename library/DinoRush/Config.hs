module DinoRush.Config where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Animate

import DinoRush.Engine.Bird
import DinoRush.Engine.Bouncer
import DinoRush.Engine.Dino
import DinoRush.Engine.Mountain
import DinoRush.Engine.Lava
import DinoRush.Engine.Rock
import DinoRush.Engine.Types

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }

data Resources = Resources
  { rMountainSprites :: Animate.SpriteSheet MountainKey SDL.Texture Seconds
  , rJungleSprites :: SDL.Texture
  , rGroundSprites :: SDL.Texture
  , rRiverSprites :: SDL.Texture
  , rDinoSprites :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  , rBirdSprites :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  , rBouncerSprites :: Animate.SpriteSheet BouncerKey SDL.Texture Seconds
  , rLavaSprites :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  , rRockSprites :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , rGameMusic :: Mixer.Music
  , rJumpSfx :: Mixer.Chunk
  , rPointSfx :: Mixer.Chunk
  }
