module DinoRush.Resource where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import qualified Animate
import Data.StateVar (($=))
import SDL.Vect

import DinoRush.Engine.Types
import DinoRush.Engine.Dino
import DinoRush.Engine.Bird
import DinoRush.Engine.Lava
import DinoRush.Engine.Mountain
import DinoRush.Engine.Rock

data Resources = Resources
  { rMountainSprites :: Animate.SpriteSheet MountainKey SDL.Texture Seconds
  , rJungleSprites :: SDL.Texture
  , rGroundSprites :: SDL.Texture
  , rRiverSprites :: SDL.Texture
  , rDinoSprites :: Animate.SpriteSheet DinoKey SDL.Texture Seconds
  , rBirdSprites :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  , rLavaSprites :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  , rRockSprites :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , rGameMusic :: Mixer.Music
  , rJumpSfx :: Mixer.Chunk
  , rDuckSfx :: Mixer.Chunk
  , rPointSfx :: Mixer.Chunk
  , rBirdSfx :: Mixer.Chunk
  , rHurtSfx :: Mixer.Chunk
  , rLavaSfx :: Mixer.Chunk
  , rQuakeSfx :: Mixer.Chunk
  , rRockSfx :: Mixer.Chunk
  , rDeathSfx :: Mixer.Chunk
  , rRecoverSfx :: Mixer.Chunk
  , rPauseSprite :: SDL.Texture
  , rSpaceSprite :: SDL.Texture
  , rEscapeSprite :: SDL.Texture
  , rGameOverSprite :: SDL.Texture
  , rHiscoreSprite :: SDL.Texture
  }

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface <- Image.load path
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  smallFont <- Font.load "resource/Computer Speak v0.3.ttf" 24
  bigFont <- Font.load "resource/Computer Speak v0.3.ttf" 64
  gameMusic <- Mixer.load "resource/v42.mod"
  jumpSfx <- Mixer.load "resource/jump.wav"
  pointSfx <- Mixer.load "resource/point.wav"
  birdSfx <- Mixer.load "resource/bird.wav"
  duckSfx <- Mixer.load "resource/duck.wav"
  hurtSfx <- Mixer.load "resource/hurt.wav"
  quakeSfx <- Mixer.load "resource/quake.wav"
  rockSfx <- Mixer.load "resource/rock.wav"
  lavaSfx <- Mixer.load "resource/lava.wav"
  deathSfx <- Mixer.load "resource/death.wav"
  recoverSfx <- Mixer.load "resource/recover.wav"
  mountainSprites <- Animate.readSpriteSheetJSON loadTexture "resource/mountain.json" :: IO (Animate.SpriteSheet MountainKey SDL.Texture Seconds)
  jungle <- loadTexture "resource/jungle.png" Nothing
  ground <- loadTexture "resource/ground.png" Nothing
  river <- loadTexture "resource/river.png" Nothing
  pauseSprite <- toTexture =<< Font.solid bigFont (V4 255 255 255 255) "PAUSED"
  spaceSprite <- toTexture =<< Font.solid smallFont (V4 255 255 255 255) "PRESS SPACE"
  escapeSprite <- toTexture =<< Font.solid smallFont (V4 255 255 255 255) "PRESS ESCAPE TO QUIT"
  gameOverSprite <- toTexture =<< Font.solid bigFont (V4 255 255 255 255) "GAME OVER"
  hiscoreSprite <- toTexture =<< Font.solid smallFont (V4 255 255 255 255) "HISCORE"
  dinoSprites <- Animate.readSpriteSheetJSON loadTexture "resource/dino.json" :: IO (Animate.SpriteSheet DinoKey SDL.Texture Seconds)
  birdSprites <- Animate.readSpriteSheetJSON loadTexture "resource/bird.json" :: IO (Animate.SpriteSheet BirdKey SDL.Texture Seconds)
  lavaSprites <- Animate.readSpriteSheetJSON loadTexture "resource/lava.json" :: IO (Animate.SpriteSheet LavaKey SDL.Texture Seconds)
  rockSprites <- Animate.readSpriteSheetJSON loadTexture "resource/rock.json" :: IO (Animate.SpriteSheet RockKey SDL.Texture Seconds)
  Font.free smallFont
  Font.free bigFont
  return Resources
    { rMountainSprites = mountainSprites
    , rJungleSprites = jungle
    , rGroundSprites = ground
    , rRiverSprites = river
    , rDinoSprites = dinoSprites
    , rBirdSprites = birdSprites
    , rLavaSprites = lavaSprites
    , rRockSprites = rockSprites
    , rJumpSfx = jumpSfx
    , rDuckSfx = duckSfx
    , rPointSfx = pointSfx
    , rBirdSfx = birdSfx
    , rHurtSfx = hurtSfx
    , rLavaSfx = lavaSfx
    , rQuakeSfx = quakeSfx
    , rRockSfx = rockSfx
    , rDeathSfx = deathSfx
    , rRecoverSfx = recoverSfx
    , rGameMusic = gameMusic
    , rPauseSprite = pauseSprite
    , rSpaceSprite = spaceSprite
    , rEscapeSprite = escapeSprite
    , rGameOverSprite = gameOverSprite
    , rHiscoreSprite = hiscoreSprite
    }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
    loadTexture path c = SDL.createTextureFromSurface renderer =<< loadSurface path c

freeResources :: Resources -> IO ()
freeResources r = do
  SDL.destroyTexture $ Animate.ssImage (rMountainSprites r)
  SDL.destroyTexture (rJungleSprites r)
  SDL.destroyTexture (rGroundSprites r)
  SDL.destroyTexture (rRiverSprites r)
  SDL.destroyTexture $ Animate.ssImage (rDinoSprites r)
  SDL.destroyTexture $ Animate.ssImage (rBirdSprites r)
  SDL.destroyTexture $ Animate.ssImage (rLavaSprites r)
  SDL.destroyTexture $ Animate.ssImage (rRockSprites r)
  SDL.destroyTexture (rPauseSprite r)
  SDL.destroyTexture (rEscapeSprite r)
  SDL.destroyTexture (rSpaceSprite r)
  SDL.destroyTexture (rGameOverSprite r)
  SDL.destroyTexture (rHiscoreSprite r)

  Mixer.free (rGameMusic r)
  Mixer.free (rJumpSfx r)
  Mixer.free (rDuckSfx r)
  Mixer.free (rPointSfx r)
  Mixer.free (rBirdSfx r)
  Mixer.free (rHurtSfx r)
  Mixer.free (rLavaSfx r)
  Mixer.free (rQuakeSfx r)
  Mixer.free (rRockSfx r)
  Mixer.free (rRecoverSfx r)
  Mixer.free (rDeathSfx r)
