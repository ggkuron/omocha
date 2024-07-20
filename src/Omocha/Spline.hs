module Omocha.Spline where

import Data.Ord (clamp)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Linear.Epsilon
import RIO hiding (trace, traceStack)

data Spline a = Spline
  { a :: a,
    b :: a,
    c :: a,
    d :: a
  }
  deriving (Show, Generic)

isLinear :: (Num a, Epsilon a) => Spline a -> Bool
isLinear sp = nearZero sp.c && nearZero sp.d

spline :: forall a. (HasCallStack, Num a, Fractional a, Storable a) => V.Vector a -> V.Vector (Spline a)
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
      c :: MVS.MVector s a <- MVS.new (length sp)
      MVS.write c 0 0
      MVS.write c num 0
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
            let c' = (cc - c1) / tmp
            MVS.write c i c'
        )
      forM_
        (reverse [1 .. num - 1])
        ( \i -> do
            c1 <- c `MVS.read` (i + 1)
            cc <- c `MVS.read` i
            wc <- w `MVS.read` i
            MVS.write c i (cc - c1 * wc)
        )
      return c

    calculateC i = cValues VS.! i
    calculateD i
      | i == len = 0
      | otherwise = (calculateC (i + 1) - calculateC i) / 3
    calculateB i
      | i == len = 0
      | otherwise = calculateA (i + 1) - calculateA i - calculateC i - calculateD i

calcSpline :: (HasCallStack, Num a) => Spline a -> a -> a
calcSpline (Spline a b c d) dt = a + (b + (c + d * dt) * dt) * dt

-- unsafe partial
findSegment :: (HasCallStack, RealFrac t) => V.Vector a -> t -> (a, t)
findSegment splines t =
  let j = clamp (0, length splines - 1) (floor t)
   in (splines V.! j, t - fromIntegral j)

-- unsafe partial
calcSplines :: forall a. (HasCallStack) => (Num a, RealFrac a, Show a) => V.Vector (Spline a) -> a -> a
calcSplines splines t =
  let (seg, t') = findSegment splines t
   in calcSpline seg t'

-- calcSplinePairs :: forall a. (HasCallStack) => (Num a, RealFrac a, Show a) => V.Vector (Spline a, Spline a) -> a -> (a, a)
-- calcSplinePairs splines t =
--   let (seg, t') = findSegment splines t
--    in both (`calcSpline` t') seg

fromAxisValues :: (Num a, Fractional a, Real b, Num c) => Vector (c, b) -> Vector a
fromAxisValues = V.map fromAxisValue

fromAxisValue :: forall a b c. (Fractional a, Real b, Num c) => (c, b) -> a
fromAxisValue = snd . second realToFrac

interpolate :: (Num a, RealFrac a, Show a) => V.Vector (Spline a) -> Int -> V.Vector a
interpolate ps divs = V.generate ((length ps - 1) * divs + 1) $ \t -> calcSplines ps (fromIntegral t * (1 / fromIntegral divs))

type Splines a = Vector (Spline a)

type SplinePair a = (Splines a, Splines a)
