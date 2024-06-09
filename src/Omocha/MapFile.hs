{-# LANGUAGE DeriveAnyClass #-}

module Omocha.MapFile where

import Data.Aeson
import Data.BoundingBox qualified as BB
import Data.Either.Combinators (maybeToRight)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear.V2
import Omocha.Spline
import RIO

type EdgePoint = (Bool, Bool)

type Direction = (Axis, Bool)

adjacentEdges :: EdgePoint -> [Direction]
adjacentEdges (True, True) = [(Y, True), (X, True)]
adjacentEdges (True, False) = [(Y, False), (X, True)]
adjacentEdges (False, True) = [(Y, True), (X, False)]
adjacentEdges (False, False) = [(Y, False), (X, False)]

data MapReference = Embed MapFile | External FilePath
  deriving (Show, Generic, ToJSON, FromJSON)

type Color = (Float, Float, Float, Float)

data MapDef
  = Cube
      { height :: Float,
        color :: Color,
        yOffset :: Float
      }
  | Plane
      { color :: Color,
        yOffset :: Float
      }
  | Slope
      { high :: Float,
        low :: Float,
        color :: Color,
        yOffset :: Float,
        highEdge :: Direction
      }
  | Reference MapReference
  | Cylinder
      { height :: Float,
        color :: Color,
        yOffset :: Float,
        center :: Maybe EdgePoint
      }
  | Cone
      { height :: Float,
        color :: Color,
        yOffset :: Float,
        center :: Maybe EdgePoint
      }
  | Sphere
      { height :: Float,
        color :: Color,
        yOffset :: Float
      }
  deriving (Show, Generic, ToJSON, FromJSON)

data MapData
  = Tips
      { maps :: Vector (Vector (Vector Int)),
        defs :: M.Map Int MapDef
      }
  | Fill MapDef
  deriving (Show, Generic, ToJSON, FromJSON)

-- axesSpline :: (HasCallStack) => Maybe Splined -> (V.Vector (Spline Float), V.Vector (Spline Float))
-- axesSpline = both toSpline
--   where
--     toSpline = spline . (fmap (v2 . first fromIntegral) . V.fromList)
--     v2 (a, b) = V2 a b

type Size = (Int, Int)

data Axis = X | Y
  deriving (Show, Generic, ToJSON, FromJSON)

data Splined = Splined
  { axis :: Axis,
    values :: [(Int, Float)]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data MapFile = MapFile
  { size :: Size,
    spline :: Maybe Splined,
    mapData :: Vector MapData
  }
  deriving (Show, Generic, ToJSON, FromJSON)

mf :: MapFile
mf =
  MapFile
    { size = (6, 5),
      spline =
        Just
          $ Splined
            Y
            [(0, 0), (2, 3)],
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
                    [ (1, Cube 1 (0.4, 0.2, 0.4, 1) 0),
                      (2, Cube 2 (0.5, 0.5, 0.5, 1) 0),
                      (3, Cube 2 (0.4, 0.8, 0.4, 1) 0),
                      (8, Cube 8 (0.5, 0.5, 0.5, 1) 0),
                      (5, Cylinder 0.01 (0.5, 0.5, 0.5, 1) 0 Nothing),
                      (8, Reference (Embed (MapFile (2, 2) Nothing V.empty)))
                    ]
              }
          ]
    }

parseMap :: (Int, Int) -> Vector (Vector Int) -> Either String (Vector (BB.Box V2 Int, Int))
parseMap (sw, sh) mapData
  | length mapData /= sh = Left "row mismatch"
  | any (\row -> length row /= sw) mapData = Left "column mismatch"
  | otherwise =
      Right
        $ foldl'
          ( \a (V2 x y) ->
              let k = l x y
                  w = ew k x y a
                  h = eh k w x y a
               in (if (k == 0) || (w == 0) then a else (BB.Box (V2 x y) (V2 (x + w) (y + h + 1)), k) `V.cons` a)
          )
          V.empty
          ([V2 x y | y <- [0 .. sh - 1], x <- [0 .. sw - 1]])
  where
    l x y = mapData V.! y V.! x
    ew v x y bbs = length (takeWhile id [v == l x' y && all (\(b, _) -> not $ isInside b x' y) bbs | x' <- [x .. sw - 1]])
    eh v w x y bbs = length (takeWhile id [ew v x y' bbs == w | w > 0, y' <- [y + 1 .. sh - 1]])

isInside :: BB.Box V2 Int -> Int -> Int -> Bool
isInside bb x y = BB.isInside (fmap fromIntegral (V2 x y) + (V2 0.5 0.5 :: V2 Float)) (fmap fromIntegral bb)

parseMapDef :: (Int, Int) -> MapData -> Either String (Vector (BB.Box V2 Int, MapDef))
parseMapDef size (Tips {..}) = do
  bmap <- mapM (parseMap size) maps
  mapM
    ( \(b, k) -> do
        def <- maybeToRight ("key: " ++ show k ++ " not found") (defs M.!? k)
        Right (b, def)
    )
    (V.concatMap id bmap)
parseMapDef (x, y) (Fill def) = Right $ V.singleton (BB.Box (V2 0 0) (V2 x y), def)

interpolate1' :: (Num a, RealFrac a, Storable a) => Size -> Maybe Splined -> (V.Vector (V2 a), V.Vector (V2 a))
interpolate1' (sx, sy) s = i1 sx *** i1 sy $ sps s
  where
    sps Nothing = (V.empty, V.empty)
    sps (Just (Splined X v)) = (V.fromList v, V.empty)
    sps (Just (Splined Y v)) = (V.empty, V.fromList v)
    i1 sz = interpolate1 . adjust sz
    adjust :: Int -> V.Vector (Int, Float) -> V.Vector (Int, Float)
    adjust sz v =
      if
        | null v -> V.empty `V.snoc` (0, 0) `V.snoc` (sz, 0)
        | length v >= 2 ->
            let v' = if fst (V.head v) /= 0 then (0, 0) `V.cons` v else v
             in if fst (V.last v) /= sz then v' `V.snoc` (sz, 0) else v'
        | otherwise -> v

spline1 :: (Num a, RealFrac a, Storable a) => Size -> Maybe Splined -> (V.Vector (Spline a), V.Vector (Spline a))
spline1 sz s = both spline $ interpolate1' sz s

pairToV2 :: (a, a) -> V2 a
pairToV2 (a, b) = V2 a b