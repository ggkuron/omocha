module Omocha.Spline where

import Data.Ord (clamp)
import Data.Vector qualified as V
import Linear.V2
import Prelude

data Spline a = Spline
  { a :: V2 a,
    b :: V2 a,
    c :: V2 a,
    d :: V2 a
  }
  deriving (Show)

spline :: V.Vector (V2 Float) -> V.Vector (Spline Float)
spline sp =
  V.generate (length sp) $ \i ->
    Spline
      { a = calculateA i,
        c = calculateC i,
        d = calculateD i,
        b = calculateB i
      }
  where
    len = length sp - 1
    calculateA i = sp V.! i
    calculateC i
      | i == 0 = 0
      | i == len = 0
      | otherwise = ((3 * (calculateA (i + 1) - 2 * calculateA i + calculateA (i - 1))) - calculateC (i - 1) - calculateC (i + 1)) / 4
    calculateD i
      | i == len = 0
      | otherwise = (calculateC (i + 1) - calculateC i) / 3
    calculateB i
      | i == len = 0
      | otherwise = calculateA (i + 1) - calculateA i - calculateC i - calculateD i

calcSplineValue :: V.Vector (Spline Float) -> Float -> V2 Float
calcSplineValue splines t =
  let n = length splines - 1
      j = clamp (0, n - 1) $ floor t -- セグメントインデックス
      Spline aj bj cj dj = splines V.! j
      dt = pure $ t - fromIntegral j -- セグメント内の相対位置
   in aj + (bj + (cj + dj * dt) * dt) * dt

drawSpline :: V.Vector (V2 Float) -> V.Vector (V2 Float)
drawSpline ps = V.generate (length ps) $ \t -> calcSplineValue (spline ps) (fromIntegral t * 0.1)