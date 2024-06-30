{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Omocha.MapFile where

import Data.Aeson
import Data.Array (Ix)
import Data.BoundingBox qualified as BB
import Data.Either.Combinators (maybeToRight)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both, third3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Linear.V2
import Linear.V3
import Omocha.Spline
import Omocha.Uniform
import RIO

type EdgePoint = (Bool, Bool)

type Direction = (Axis, Bool)

adjacentEdges :: EdgePoint -> Vector Direction
adjacentEdges (True, True) = V.fromList [(Y, True), (X, True)]
adjacentEdges (True, False) = V.fromList [(Y, False), (X, True)]
adjacentEdges (False, True) = V.fromList [(Y, True), (X, False)]
adjacentEdges (False, False) = V.fromList [(Y, False), (X, False)]

data MapReference = Embed (MapFile MapDef) | External FilePath
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Color = (Float, Float, Float, Float)

type RGBA = (Int, Int, Int, Int)

rgba :: RGBA -> Color
rgba (r, g, b, a) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, fromIntegral a / 255)

data Meta
  = SplineStart
      { direction :: Direction,
        search :: Direction,
        tip :: DefId,
        cross :: Int
      }
  | SplinePoint
      { search :: Direction,
        tip :: DefId,
        cross :: Int
      }
  | SplineEnd
      { direction :: Direction,
        tip :: DefId
      }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data MapDef
  = Empty
  | Cube
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Plane
      { color :: RGBA,
        yOffset :: Float
      }
  | Slope
      { high :: Float,
        low :: Float,
        color :: RGBA,
        yOffset :: Float,
        highEdge :: Direction
      }
  | Tetra
      { high :: Float,
        low :: Float,
        color :: RGBA,
        yOffset :: Float,
        edge :: EdgePoint
      }
  | Reference MapReference Float
  | Cylinder
      { height :: Float,
        color :: RGBA,
        yOffset :: Float,
        center :: Maybe EdgePoint
      }
  | Cone
      { height :: Float,
        color :: RGBA,
        yOffset :: Float,
        center :: Maybe EdgePoint
      }
  | Sphere
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  deriving (Show, Generic, Eq, Ord, ToJSON, FromJSON)

data MapData a
  = Fill a
  | Tips
      { maps :: Vector (Vector (Vector DefId)),
        defs :: M.Map DefId a
      }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

vectorToMaybe :: Vector a -> Maybe a
vectorToMaybe = V.foldr (const . Just) Nothing
{-# INLINE vectorToMaybe #-}

type Size a = (a, a)

data Axis = X | Y
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Spline

data Splined = Splined
  { start :: [(Int, Float)],
    end :: Maybe [(Int, Float)]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MapFile a = MapFile
  { size :: Size Int,
    splineX :: Maybe Splined,
    splineY :: Maybe Splined,
    mapData :: Vector (MapData a)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype DefId = DefId Int
  deriving (Generic)
  deriving (Eq, Show, Num, Ord, Ix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via Int

newtype InstanceId = InstanceId Int
  deriving (Generic)
  deriving (Eq, Show, Num, Ord, Ix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via Int

mf :: MapFile MapDef
mf =
  MapFile
    { size = (6, 5),
      splineY =
        Just
          $ Splined
            [(0, 0), (2, -3)]
            (Just [(0, 0), (2, 3)]),
      splineX = Nothing,
      mapData =
        V.fromList
          [ Tips
              { maps =
                  V.singleton
                    . V.fromList
                    . fmap V.fromList
                    $ [ [0, 0, 0, 0, 0, 0],
                        [1, 1, 0, 0, 0, 0],
                        [1, 1, 0, 3, 3, 3],
                        [0, 2, 0, 5, 0, 0],
                        [0, 8, 0, 0, 0, 0]
                      ],
                defs =
                  M.fromList
                    [ (1, Cube 1 (100, 30, 100, 255) 0),
                      (2, Cube 2 (127, 127, 127, 255) 0),
                      (3, Cube 2 (100, 200, 100, 255) 0),
                      (8, Cube 8 (127, 127, 127, 255) 0),
                      (5, Cylinder 0.01 (127, 127, 127, 255) 0 Nothing),
                      (8, Reference (Embed (MapFile (2, 2) Nothing Nothing V.empty)) 0)
                    ]
              }
          ]
    }

parseMap :: (HasCallStack) => (Int, Int) -> Vector (Vector DefId) -> Either String (Vector (BB.Box V2 Int, DefId))
parseMap (sw, sh) mapData
  | length mapData /= sh = Left "row mismatch"
  | any (\row -> length row /= sw) mapData = Left "column mismatch"
  | otherwise =
      Right
        $ foldl'
          ( \a (V2 x y) ->
              let k@(DefId ik) = l x y
                  w = ew k x y a
                  h = eh k w x y a
               in (if (ik == 0) || (w == 0) then a else (BB.Box (V2 x y) (V2 (x + w) (y + h + 1)), k) `V.cons` a)
          )
          V.empty
          ([V2 x y | y <- [0 .. sh - 1], x <- [0 .. sw - 1]])
  where
    l x y = mapData V.! y V.! x
    ew v x y bbs = length (takeWhile id [v == l x' y && all (\(b, _) -> not $ isInside b x' y) bbs | x' <- [x .. sw - 1]])
    eh v w x y bbs = length (takeWhile id [ew v x y' bbs == w | w > 0, y' <- [y + 1 .. sh - 1]])

isInside :: BB.Box V2 Int -> Int -> Int -> Bool
isInside bb x y = BB.isInside (fmap fromIntegral (V2 x y) + (V2 0.5 0.5 :: V2 Float)) (fmap fromIntegral bb)

newtype MapDefs a b = MapDefs
  { defs :: M.Map ObjectId (a, Size Int, b)
  }
  deriving (Show)

instance Monoid (MapDefs MapDef (Vector a)) where
  mempty = MapDefs M.empty

data Maps = Maps
  { unit :: V2 Float,
    defs :: Vector (BB.Box V2 Int, MapDef, ObjectId),
    offset :: V3 Float,
    splines :: SplinePairs Float
  }

instance Semigroup (MapDefs MapDef (Vector a)) where
  (MapDefs im) <> (MapDefs im') = MapDefs (im `M.union` M.mapKeys (+ leftMaxId) im')
    where
      leftMaxId = M.foldlWithKey' (\a i _ -> max a i) 1 im

parse :: (HasCallStack) => forall a. (Int, Int) -> MapData a -> Either String (Vector (BB.Box V2 Int, a, ObjectId))
parse (x, y) (Fill def) = Right $ V.singleton (BB.Box (V2 0 0) (V2 x y), def, ObjectId 1)
parse size (Tips {..}) = do
  b <- foldMapM (parseMap size) maps
  V.imapM
    ( \i (bb, k) -> do
        def <- maybeToRight ("key: " ++ show k ++ " not found") (defs M.!? k)
        return (bb, def, ObjectId i + 1)
    )
    b

parseMapDef :: (HasCallStack) => forall a. (Int, Int) -> MapData a -> Either String (Vector (BB.Box V2 Int, a, ObjectId), MapDefs a (Vector (V2 Int, ObjectId)))
parseMapDef size md = do
  ls <- parse size md
  return
    ( ls,
      MapDefs
        ( V.foldl'
            ( \im (BB.Box start end, def, objId) ->
                let V2 w h = end - start
                    size = (w, h)
                 in M.alter (Just . maybe (def, size, V.singleton (start, objId)) (third3 (`V.snoc` (start, objId)))) objId im
            )
            M.empty
            ls
        )
    )

adjust :: Int -> Vector (Int, Float) -> V.Vector (Int, Float)
adjust sz v =
  if
    | null s -> (0, 0) `V.cons` v `V.snoc` (sz, 0)
    | (fst (V.head s) == 0) && (fst (V.last s) == sz) -> v
    | (fst (V.head s) /= 0) && (fst (V.last s) == sz) -> (0, snd $ V.head s) `V.cons` s
    | (fst (V.head s) == 0) && (fst (V.last s) /= sz) -> s `V.snoc` (sz, snd $ V.last s)
    | otherwise -> (0, snd $ V.head s) `V.cons` s `V.snoc` (sz, snd $ V.last s)
  where
    s = V.modify (VAI.sortBy (compare `on` fst)) (V.filter ((\a -> a >= 0 && a <= sz) . fst) v)

pointSplines :: Int -> Vector (Int, Float) -> Splines Float
pointSplines sz p =
  spline (interpolate1 (adjust sz p))

spline1 :: (HasCallStack) => Size Int -> Maybe Splined -> Maybe Splined -> SplinePairs Float
spline1 (sx, sy) Nothing Nothing =
  let xs = pointSplines sx V.empty
      ws = xs
      ys = pointSplines sy V.empty
      hs = ys
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v Nothing)) Nothing =
  let xs = pointSplines sx (V.fromList v)
      ws = xs
      ys = pointSplines sy V.empty
      hs = ys
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v (Just w))) Nothing =
  let xs = pointSplines sx (V.fromList v)
      ws = pointSplines sx (V.fromList w)
      ys = pointSplines sy V.empty
      hs = ys
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) Nothing (Just (Splined v (Just w))) =
  let xs = pointSplines sx V.empty
      ws = xs
      ys = pointSplines sy (V.fromList v)
      hs = pointSplines sy (V.fromList w)
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) Nothing (Just (Splined v Nothing)) =
  let xs = pointSplines sx V.empty
      ws = xs
      ys = pointSplines sy (V.fromList v)
      hs = ys
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v (Just w))) (Just (Splined s (Just t))) =
  let xs = pointSplines sx (V.fromList v)
      ws = pointSplines sx (V.fromList w)
      ys = pointSplines sy (V.fromList s)
      hs = pointSplines sy (V.fromList t)
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v Nothing)) (Just (Splined s (Just t))) =
  let xs = pointSplines sx (V.fromList v)
      ws = pointSplines sx V.empty
      ys = pointSplines sy (V.fromList s)
      hs = pointSplines sy (V.fromList t)
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v (Just w))) (Just (Splined s Nothing)) =
  let xs = pointSplines sx (V.fromList v)
      ws = pointSplines sx (V.fromList w)
      ys = pointSplines sy (V.fromList s)
      hs = pointSplines sy V.empty
   in (V.zip xs ws, V.zip ys hs)
spline1 (sx, sy) (Just (Splined v Nothing)) (Just (Splined s Nothing)) =
  let xs = pointSplines sx (V.fromList v)
      ws = pointSplines sx V.empty
      ys = pointSplines sy (V.fromList s)
      hs = pointSplines sy V.empty
   in (V.zip xs ws, V.zip ys hs)

splined :: (HasCallStack) => SplinePairs Float -> V2 Float -> V2 Float
splined (xsp, ysp) (V2 x y) = V2 x rx + V2 ry y
  where
    (sx, sy) = both (fromIntegral . length) (xsp, ysp)
    perY = x / sx
    perX = y / sy
    (rx1, rx2) = calcSplinePairs xsp x
    (ry1, ry2) = calcSplinePairs ysp y
    rx = rx1 * (1 - perX) - rx2 * perX
    ry = ry1 * (1 - perY) - ry2 * perY

splined3 :: (HasCallStack) => SplinePairs Float -> V3 Float -> V3 Float
splined3 sps (V3 x y z) =
  let V2 x' z' = splined sps (V2 x z)
   in V3 x' y z'

interpolate1 :: V.Vector (Int, Float) -> V.Vector Float
interpolate1 ps | null ps = V.empty
interpolate1 ps =
  V.concatMap
    ( \(i, d) -> V.generate d
        $ \t ->
          let t' :: Float = (fromIntegral i + fromIntegral t / fromIntegral d)
              sps = (spline . fromAxisValues $ ps)
              v = calcSplines sps t'
           in v
    )
    (V.izipWith (\i a b -> (i, a - b)) (V.tail xs) xs)
    `V.snoc` (fromAxisValue . V.last) ps
  where
    xs = V.map fst ps
