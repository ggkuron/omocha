module Omocha.Spline where

import Data.Ord (clamp)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Linear.Epsilon
import Linear.V2
import Linear.V3
import RIO hiding (trace, traceStack)

data Spline a = Spline
  { a :: V2 a,
    b :: V2 a,
    c :: V2 a,
    d :: V2 a
  }
  deriving (Show, Generic)

isLinear :: (Num a, Epsilon a) => Spline a -> Bool
isLinear sp = nearZero sp.c && nearZero sp.d

spline :: (HasCallStack, Num a, Fractional a, Storable a) => V.Vector (V2 a) -> V.Vector (Spline a)
spline sp | length sp < 2 = V.empty
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
    cValues = VS.create $ do
      let num = length sp - 1
      c :: MVS.MVector s (V2 a) <- MVS.new (length sp)
      MVS.write c 0 (V2 0 0)
      MVS.write c num (V2 0 0)
      forM_
        [1 .. num - 1]
        (\i -> MVS.write c i (3 * (calculateA (i - 1) - 2 * calculateA i + calculateA (i + 1))))
      w :: MVS.MVector s a <- MVS.new (length sp)
      _ <- MVS.write w 0 0
      forM_
        [1 .. num - 1]
        ( \i -> do
            w1 <- w `MVS.read` (i - 1)
            let tmp = 4 - w1
            MVS.write w i (1 / tmp)
            c1 <- c `MVS.read` (i - 1)
            cc <- c `MVS.read` i
            let c' = (cc - c1) / pure tmp
            MVS.write c i c'
        )
      forM_
        (reverse [1 .. num - 1])
        ( \i -> do
            c1 <- c `MVS.read` (i + 1)
            cc <- c `MVS.read` i
            wc <- w `MVS.read` i
            MVS.write c i (cc - c1 * pure wc)
        )
      return c

    calculateC i = cValues VS.! i
    calculateD i
      | i == len = 0
      | otherwise = (calculateC (i + 1) - calculateC i) / 3
    calculateB i
      | i == len = 0
      | otherwise = calculateA (i + 1) - calculateA i - calculateC i - calculateD i

calcSpline :: (HasCallStack, Num a) => Spline a -> a -> V2 a
calcSpline (Spline a b c d) t = a + (b + (c + d * dt) * dt) * dt
  where
    dt = pure t

-- unsafe partial
findSegment :: (Num a, RealFrac a) => V.Vector (Spline a) -> a -> (Spline a, a)
findSegment splines t =
  let j = clamp (0, length splines - 1) (floor t)
   in (splines V.! j, t - fromIntegral j)

-- unsafe partial
calcSplines :: (Num a, RealFrac a) => V.Vector (Spline a) -> a -> V2 a
calcSplines splines t =
  let (seg, t') = findSegment splines t
   in calcSpline seg t'

interpolate1 :: (Num a, RealFrac a, Storable a) => V.Vector (Int, Float) -> V.Vector (V2 a)
interpolate1 ps | null ps = V.empty
interpolate1 ps =
  V.concatMap (\(i, d) -> V.generate d $ \t -> calcSplines ss (fromIntegral i + fromIntegral t / fromIntegral d)) ds
    `V.snoc` (fromAxisValue . V.last) ps
  where
    ss = spline . fromAxisValues $ ps
    xs = V.map fst ps
    ds = V.izipWith (\i a b -> (i, a - b)) (V.tail xs) xs

fromAxisValues :: (Fractional a) => Vector (Int, Float) -> Vector (V2 a)
fromAxisValues = V.map fromAxisValue

fromAxisValue :: (Fractional a) => (Int, Float) -> V2 a
fromAxisValue = pairToV2 . (fromIntegral *** realToFrac)
  where
    pairToV2 :: (a, a) -> V2 a
    pairToV2 (a, b) = V2 a b

interpolate :: (Num a, RealFrac a) => V.Vector (Spline a) -> V.Vector (V2 a)
interpolate ps = V.generate ((length ps - 1) * 4 + 1) $ \t -> calcSplines ps (fromIntegral t * 0.25)

type Splines a = (Vector (Spline a), Vector (Spline a))

splined2 :: Splines Float -> V2 Float -> V2 Float
splined2 sps (V2 x y) =
  let x2 = calcSplines (fst sps) x
      y2 = calcSplines (snd sps) y ^. _yx
   in x2 + y2

splined3 :: Splines Float -> V3 Float -> V3 Float
splined3 sps (V3 x y z) =
  let V2 x' z' = splined2 sps (V2 x z)
   in V3 x' y z'
