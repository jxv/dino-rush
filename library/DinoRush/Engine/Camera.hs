module DinoRush.Engine.Camera where

import Linear

data Camera = Camera
  { camOrigin :: V2 Float
  , camZoom :: V2 Float
  } deriving (Show, Eq)

screenWidth, screenHeight :: Float
screenWidth = 1280
screenHeight = 720

screenV2 :: V2 Float
screenV2 = V2 screenWidth screenHeight

initCamera :: Camera
initCamera = Camera (V2 (screenWidth / 2) (screenHeight / 2)) (V2 1 1)

moveOrigin :: V2 Float -> V2 Float
moveOrigin (V2 x y) = V2 (screenWidth / 2 - x) (screenHeight / 2 - y)

lerpCamera :: Float -> Camera -> Camera -> Camera
lerpCamera p a b = Camera (lerp p (camOrigin a) (camOrigin b)) (lerp p (camZoom a) (camZoom b))
