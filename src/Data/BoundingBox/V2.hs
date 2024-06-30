module Data.BoundingBox.V2 where

import Data.BoundingBox qualified as BB
import Linear.V2
import RIO

center :: BB.Box V2 Float -> V2 Float
center (BB.Box (V2 x y) (V2 x' y')) = V2 ((x + x') / 2) ((y + y') / 2)

move :: BB.Box V2 Float -> V2 Float -> BB.Box V2 Float
move (BB.Box (V2 x y) (V2 x' y')) (V2 x'' y'') = BB.Box (V2 (x + x'') (y + y'')) (V2 (x' + x'') (y' + y''))

mult :: (Num a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
mult (BB.Box (V2 x y) (V2 x' y')) (V2 ux uy) = BB.Box (V2 (ux * x) (uy * y)) (V2 (ux * x') (uy * y'))

divide :: (Fractional a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
divide (BB.Box (V2 x y) (V2 x' y')) (V2 ux uy) = BB.Box (V2 (ux / x) (uy / y)) (V2 (ux / x') (uy / y'))

aabb :: (Ord a) => BB.Box V2 a -> BB.Box V2 a -> Bool
aabb (BB.Box amin amax) (BB.Box bmin bmax) =
  amin
    ^. _x <= bmax
    ^. _x
      && amax
    ^. _x >= bmin
    ^. _x
      && amin
    ^. _y <= bmax
    ^. _y
      && amax
    ^. _y >= bmin
    ^. _y
