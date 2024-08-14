module Data.BoundingBox.V2 where

import Data.BoundingBox qualified as BB
import Data.Vector qualified as V
import Linear.Epsilon
import Linear.Metric (dot, normalize)
import Linear.V2
import RIO
import Prelude (maximum, minimum)

center :: BB.Box V2 Float -> V2 Float
center (BB.Box (V2 x y) (V2 x' y')) = V2 ((x + x') / 2) ((y + y') / 2)

move :: (Num a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
move (BB.Box (V2 x y) (V2 x' y')) (V2 x'' y'') = BB.Box (V2 (x + x'') (y + y'')) (V2 (x' + x'') (y' + y''))

mult :: (Num a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
mult (BB.Box (V2 x y) (V2 x' y')) (V2 ux uy) = BB.Box (V2 (ux * x) (uy * y)) (V2 (ux * x') (uy * y'))

divide :: (Fractional a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
divide (BB.Box (V2 x y) (V2 x' y')) (V2 ux uy) = BB.Box (V2 (ux / x) (uy / y)) (V2 (ux / x') (uy / y'))

move' :: (Num a, Ord a) => BB.Box V2 a -> V2 a -> BB.Box V2 a
move' (BB.Box p q) v@(V2 x y) | x < 0 && y < 0 = BB.Box ((+ v) p) q
move' (BB.Box p q) (V2 x y) | x < 0 && y > 0 = BB.Box ((+ V2 x 0) p) ((+ V2 0 y) q)
move' (BB.Box p q) (V2 x y) | y < 0 && x > 0 = BB.Box ((+ V2 0 y) p) ((+ V2 x 0) q)
move' (BB.Box p q) v = BB.Box p ((+ v) q)

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

data Box' a = Box' (V2 a) (V2 a) (V2 a) (V2 a) deriving (Show)

-- 投影軸に対する四角形の頂点の投影
project :: (Ord a, Floating a) => V2 a -> Box' a -> (a, a)
project axis (Box' v1 v2 v3 v4) = (minimum ps, maximum ps)
  where
    ps = V.map (dot axis) $ V.fromList [v1, v2, v3, v4]

-- 投影の重なり判定
overlaps :: (Ord a) => (a, a) -> (a, a) -> Bool
overlaps (minA, maxA) (minB, maxB) = maxA >= minB && maxB >= minA

-- 四角形の辺を取得
edges :: (Floating a) => Box' a -> Vector (V2 a)
edges (Box' v1 v2 v3 v4) = V.fromList [v2 - v1, v3 - v2, v4 - v3, v1 - v4]

-- 四角形の重なり判定
aabb' :: (Ord a, Floating a, Epsilon a) => Box' a -> Box' a -> Bool
aabb' boxA boxB = all checkAxis (axes boxA boxB)
  where
    checkAxis axis = overlaps (project axis boxA) (project axis boxB)
    axes a b = V.map (normalize . perp) (edges a V.++ edges b)
    perp (V2 x y) = V2 (-y) x
