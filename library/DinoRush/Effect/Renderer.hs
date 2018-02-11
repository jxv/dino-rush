module DinoRush.Effect.Renderer where

import qualified Animate
import qualified SDL
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader

import DinoRush.Config
import DinoRush.Engine.Types
import DinoRush.Entity.Dino
import DinoRush.Entity.Lava
import DinoRush.Entity.Rock
import DinoRush.Entity.Bird
import DinoRush.Entity.Bouncer
import DinoRush.Entity.Mountain
import DinoRush.Wrapper.SDLRenderer

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  getDinoAnimations :: m (Animations DinoKey)
  getLavaAnimations :: m (Animations LavaKey)
  getRockAnimations :: m (Animations RockKey)
  getBirdAnimations :: m (Animations BirdKey)
  getBouncerAnimations :: m (Animations BouncerKey)
  getMountainAnimations :: m (Animations MountainKey)
  drawDino :: DrawSprite DinoKey m
  drawLava :: DrawSprite LavaKey m
  drawRock :: DrawSprite RockKey m
  drawBird :: DrawSprite BirdKey m
  drawBouncer :: DrawSprite BouncerKey m
  drawMountain :: DrawSprite MountainKey m
  drawJungle :: (Int, Int) -> m ()
  drawGround :: (Int, Int) -> m ()
  drawRiver :: (Int, Int) -> m ()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  renderer <- asks cRenderer
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  presentRenderer renderer

--

mountainY, jungleY, groundY, riverY :: Int
mountainY = -16
jungleY = 16 * 16
groundY = 16 * 28
riverY = 16 * 36

--

drawSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawSprite ss clip (x,y) = do
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

drawHorizontalScrollSprite :: (MonadReader Config m, SDLRenderer m) => (Config ->  Animate.SpriteSheet key SDL.Texture Seconds) -> Int -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawHorizontalScrollSprite ss scale clip (x,y) = do
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let dim' = fromIntegral scale *^ dim
  drawTexture renderer sheet (Just clip') (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim')
  drawTexture renderer sheet (Just clip') (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y)) dim')

getSpriteAnimations :: (MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> m (Animations key)
getSpriteAnimations ss = asks (Animate.ssAnimations . ss)

drawHorizontalScrollImage :: (MonadReader Config m, SDLRenderer m) => (Config -> SDL.Texture) -> (Int, Int) -> m ()
drawHorizontalScrollImage getTex (x,y) = do
  renderer <- asks cRenderer
  tex <- asks getTex
  SDL.TextureInfo{textureWidth,textureHeight} <- queryTexture tex
  let dim = SDL.V2 textureWidth textureHeight
  drawTexture renderer tex Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)
  drawTexture renderer tex Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y)) dim)
