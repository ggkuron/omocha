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
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Color = (Float, Float, Float, Float)

data Meta
  = SplineStart
      { direction :: Direction,
        search :: Direction,
        tip :: Int,
        cross :: Int
      }
  | SplinePoint
      { search :: Direction,
        tip :: Int,
        cross :: Int
      }
  | SplineEnd
      { direction :: Direction,
        tip :: Int
      }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

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
  | Meta Meta
  deriving (Show, Generic, Eq, Ord, ToJSON, FromJSON)

data MapData
  = Fill MapDef
  | Tips
      { maps :: Vector (Vector (Vector DefId)),
        defs :: M.Map DefId MapDef
      }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- readMeta :: MapData -> BB.Box V2 Int -> Meta -> Maybe MapDef
-- readMeta (Fill _) _ _ = Nothing
-- readMeta _ _ SplinePoint {} = Nothing
-- readMeta _ _ SplineEnd {} = Nothing
-- readMeta tps@(Tips maps defs) (BB.Box (V2 x y) (V2 x' y')) (SplineStart _ _ tip cross) = Nothing
--   where
--     isSpline (Meta SplinePoint {}) = True
--     isSpline (Meta SplineStart {}) = True
--     isSpline (Meta SplineEnd {}) = True
--     isSpline _ = False
--     find y pred =
--       listToMaybe
--         . V.toList
--         $ V.concatMap
--           ( V.take 1
--               . V.filter isJust
--               . V.imap
--                 ( \x id -> do
--                     d <- defs M.!? id
--                     if isSpline d then Just ((x, y), d) else Nothing
--                 )
--               . pred
--           )
--           maps
--     search (X, True) offset = do
--       let searchY = y + offset
--       Just (idx, def) <- find (\ls -> V.drop (x + 1) =<< (V.fromList . maybeToList $ ls V.!? searchY))
--       readMeta tps
--     search (X, False) offset = do
--       let searchY = y + offset
--       Just (idx, def) <- find (\ls -> V.take x =<< (V.fromList . maybeToList $ ls V.!? searchY))
--       return def
--     search (Y, True) offset = undefined
--     -- do
--     --   let searchX = x + offset
--     --   find (V.take x)
--     --   listToMaybe
--     --     . V.toList
--     --     $ V.concatMap
--     --       ( V.take 1
--     --           . V.filter isJust
--     --           . V.imap
--     --             ( \x id -> do
--     --                 d <- defs M.!? id
--     --                 if isSpline d then Just (x, d) else Nothing
--     --             )
--     --           (V.filter isJust (V.imap (\i ls -> (i,) <$> ls V.!? searchX ) ))
--     --       )
--     --       maps
--
--     search (Y, False) offset = undefined

type Size = (Int, Int)

data Axis = X | Y
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Splined = Splined
  { axis :: Axis,
    values :: [(Int, Float)]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MapFile = MapFile
  { size :: Size,
    spline :: Maybe Splined,
    mapData :: Vector MapData
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype DefId = DefId Int
  deriving (Generic)
  deriving (Eq, Show, Ord, Ix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via Int

newtype InstanceId = InstanceId Int
  deriving (Generic)
  deriving (Eq, Show, Num, Ord, Ix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via Int

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
                    $ [ [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
                        [DefId 1, DefId 1, DefId 0, DefId 0, DefId 0, DefId 0],
                        [DefId 1, DefId 1, DefId 0, DefId 3, DefId 3, DefId 3],
                        [DefId 0, DefId 2, DefId 0, DefId 5, DefId 0, DefId 0],
                        [DefId 0, DefId 8, DefId 0, DefId 0, DefId 0, DefId 0]
                      ],
                defs =
                  M.fromList
                    [ (DefId 1, Cube 1 (0.4, 0.2, 0.4, 1) 0),
                      (DefId 2, Cube 2 (0.5, 0.5, 0.5, 1) 0),
                      (DefId 3, Cube 2 (0.4, 0.8, 0.4, 1) 0),
                      (DefId 8, Cube 8 (0.5, 0.5, 0.5, 1) 0),
                      (DefId 5, Cylinder 0.01 (0.5, 0.5, 0.5, 1) 0 Nothing),
                      (DefId 8, Reference (Embed (MapFile (2, 2) Nothing V.empty)))
                    ]
              }
          ]
    }

parseMap :: (Int, Int) -> Vector (Vector DefId) -> Either String (Vector (BB.Box V2 Int, DefId))
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

-- type MapDefs = (Vector (BB.Box V2 Int, MapDef))
newtype MapDefs = MapDefs
  { defs :: M.Map InstanceId (MapDef, Size, Vector (V2 Int))
  }
  deriving (Show)

-- mapDefs :: MapDefs -> (InstanceId -> MapDef -> Size -> Vector ) -> b
-- M.foldMapWithKey

instance Monoid MapDefs where
  mempty = MapDefs M.empty

instance Semigroup MapDefs where
  (MapDefs im) <> (MapDefs im') = MapDefs (im `M.union` M.mapKeys (+ leftMaxId) im')
    where
      leftMaxId = M.foldlWithKey' (\a i _ -> max a i) 1 im

parseMapDef :: (Int, Int) -> MapData -> Either String MapDefs
parseMapDef size (Tips {..}) = do
  bmap <- mapM (parseMap size) maps
  ls <-
    mapM
      ( \(BB.Box start end, k) -> do
          let V2 w h = end - start
          def <- maybeToRight ("key: " ++ show k ++ " not found") (defs M.!? k)
          return (start, (w, h), def)
      )
      (join bmap)
  return
    $ MapDefs
      ( foldl'
          ( \im (i, (pos, size, def)) ->
              let id = InstanceId i
               in M.alter (Just . maybe (def, size, V.singleton pos) (third3 (`V.snoc` pos))) id im
          )
          M.empty
          (V.imap (,) ls)
      )
parseMapDef (x, y) (Fill def) = Right $ MapDefs (M.singleton 0 (def, (x, y), V.singleton (V2 0 0)))

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
