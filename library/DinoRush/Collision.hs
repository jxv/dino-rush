module DinoRush.Collision where

import Linear.V2

data Aabb = Aabb
  { aMin :: V2 Float
  , aMax :: V2 Float
  }

intersect :: Aabb -> Aabb -> Bool
intersect (Aabb (V2 al au) (V2 ar ad)) (Aabb (V2 bl bu) (V2 br bd)) = ar >= bl && al <= br && ad >= bu && au <= bd
