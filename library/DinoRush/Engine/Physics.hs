module DinoRush.Engine.Physics where

import Linear.V2

data Aabb = Aabb
  { aMin :: V2 Float
  , aMax :: V2 Float
  }

collisionIntersect :: Aabb -> Aabb -> Bool
collisionIntersect (Aabb (V2 al au) (V2 ar ad)) (Aabb (V2 bl bu) (V2 br bd)) = ar >= bl && al <= br && ad >= bu && au <= bd

characterToAabb :: Maybe Float -> Aabb
characterToAabb = \case
  Nothing -> base
  Just _height -> base
  where
    base = Aabb (V2 0 0) (V2 48 48)

groundShortAabb :: Aabb
groundShortAabb = Aabb (V2 0 47) (V2 32 79)

groundTallAabb :: Aabb
groundTallAabb = Aabb (V2 0 (16)) (V2 32 48)

airAabb :: Aabb
airAabb = Aabb (V2 0 (-20)) (V2 32 12)

bouncyAabb :: Float -> Aabb
bouncyAabb height = Aabb (V2 0 (16 + height)) (V2 32 (48 + height))

arenaWidth :: Float
arenaWidth = 1280
