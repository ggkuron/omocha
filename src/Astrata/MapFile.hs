{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Astrata.MapFile where

import Astrata.Spline
import Astrata.Uniform
import Data.Aeson
import Data.Array (Ix)
import Data.Either.Combinators (maybeToRight)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (third3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Linear.V2
import Linear.V3
import RIO

type EdgePoint = (Bool, Bool)

-- data MapReference = Embed MapFile | External FilePath
--   deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type Color = (Float, Float, Float, Float)

type RGBA = (Int, Int, Int, Int)

rgba :: RGBA -> Color
rgba (r, g, b, a) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, fromIntegral a / 255)

data CardId
  = Origin0
  | Origin1
  | Origin2
  | Origin3
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  deriving (Show, Generic, Eq, Ord, ToJSON, FromJSON)

data CardAction
  = Draw Int
  | Discard Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data OriginLevel = L0 | L1 | L2 | L3
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

type Optionality = Bool

data BattlerStatus
  = Normal
  | Tired
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data Phase
  = StartPhase
  | ActionPhase
  | AttackPhase
  | EndPhase
  deriving (Show, ToJSON, FromJSON, Generic)

data Battler = Battler
  { atk :: Int,
    def :: Int,
    status :: BattlerStatus,
    effects :: [(Phase, [CardAction], Optionality)],
    id :: (CardId, Int)
  }
  deriving (Show, ToJSON, FromJSON, Generic)

data OriginSlot = OriginSlot
  { level :: OriginLevel,
    hp :: Int,
    status :: BattlerStatus,
    planet :: Planet
  }
  deriving (Show, ToJSON, FromJSON, Generic)

data SatelliteSlot a = EmptySlot | SealedSlot | Assigned a
  deriving (Show, ToJSON, FromJSON, Generic, Functor)

data Planet = Mercury
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

battler :: SatelliteSlot Battler -> Maybe Battler
battler (Assigned b) = Just b
battler _ = Nothing

data FieldSide = Own | Rival
  deriving (Eq, Ord, Show)

origin :: FieldSide -> OriginSlot -> Battler
origin s (OriginSlot {planet = Mercury, level = L0, status}) = Battler {atk = 1, def = 1, id = (Origin0, originCardId s), status = status, effects = [(StartPhase, [], True)]}
origin s (OriginSlot {planet = Mercury, level = L1, status}) = Battler {atk = 1, def = 1, id = (Origin1, originCardId s), status = status, effects = [(StartPhase, [], True)]}
origin s (OriginSlot {planet = Mercury, level = L2, status}) = Battler {atk = 1, def = 1, id = (Origin2, originCardId s), status = status, effects = [(StartPhase, [], True)]}
origin s (OriginSlot {planet = Mercury, level = L3, status}) = Battler {atk = 1, def = 1, id = (Origin3, originCardId s), status = status, effects = [(StartPhase, [], True)]}

originCardId Own = 0
originCardId Rival = 1

data Field = Field
  { origin :: OriginSlot,
    frontL :: SatelliteSlot Battler,
    frontR :: SatelliteSlot Battler,
    backL :: SatelliteSlot Battler,
    backR :: SatelliteSlot Battler,
    cemetry :: V.Vector Battler
  }
  deriving (Show, ToJSON, FromJSON, Generic)

data BattleField = BattleField
  { own :: Field,
    rival :: Field,
    ownHands :: V.Vector CardId,
    rivalHands :: V.Vector CardId
  }
  deriving (Show, ToJSON, FromJSON, Generic)

data ShapeDef
  = Empty
  | Card
      { height :: Float,
        yOffset :: Float,
        id :: Maybe CardId
      }
  | Cube
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Plane
      { color :: RGBA,
        yOffset :: Float
      }
  | -- | Reference MapReference Float
    Sphere
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Gltf FilePath
  | Glb FilePath
  deriving (Show, Generic, Eq, Ord, ToJSON, FromJSON)

-- data MapData
--   = Fill MapDef
--   | Tips
--       { maps :: Vector (Vector (Vector DefId)),
--         defs :: M.Map DefId MapDef
--       }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

vectorToMaybe :: Vector a -> Maybe a
vectorToMaybe = V.foldr (const . Just) Nothing
{-# INLINE vectorToMaybe #-}

type Size a = (a, a)

data Axis = X | Y
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- data MapFile = MapFile
--   { size :: Size Int,
--     mapData :: Vector MapData
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- newtype DefId = DefId Int
--   deriving (Generic)
--   deriving (Eq, Show, Num, Ord, Ix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via Int

-- parseMap :: (HasCallStack) => (Int, Int) -> Vector (Vector DefId) -> Either String (Vector (BB.Box V2 Int, DefId))
-- parseMap (sw, sh) mapData
--   | length mapData /= sh = Left "row mismatch"
--   | any (\row -> length row /= sw) mapData = Left "column mismatch"
--   | otherwise =
--       Right
--         $ foldl'
--           ( \a (V2 x y) ->
--               let k@(DefId ik) = l x y
--                   w = ew k x y a
--                   h = eh k w x y a
--                in (if (ik == 0) || (w == 0) then a else (BB.Box (V2 x y) (V2 (x + w) (y + h + 1)), k) `V.cons` a)
--           )
--           V.empty
--           ([V2 x y | y <- [0 .. sh - 1], x <- [0 .. sw - 1]])
--   where
--     l x y = mapData V.! y V.! x
--     ew v x y bbs = length (takeWhile id [v == l x' y && all (\(b, _) -> not $ isInside b x' y) bbs | x' <- [x .. sw - 1]])
--     eh v w x y bbs = length (takeWhile id [ew v x y' bbs == w | w > 0, y' <- [y + 1 .. sh - 1]])
--
-- isInside :: BB.Box V2 Int -> Int -> Int -> Bool
-- isInside bb x y = BB.isInside (fmap fromIntegral (V2 x y) + (V2 0.5 0.5 :: V2 Float)) (fmap fromIntegral bb)

-- newtype MapDefs b = MapDefs
--   { defs :: M.Map ObjectId (ShapeDef, Size Int, b)
--   }
--   deriving (Show)
--
-- instance Monoid (MapDefs (Vector a)) where
--   mempty = MapDefs M.empty

-- data Maps = Maps
--   { unit :: V2 Float,
--     defs :: Vector (BB.Box V2 Int, ShapeDef, ObjectId),
--     offset :: V3 Float,
--     size :: V2 Int,
--     dir :: FilePath
--   }

-- instance Semigroup (MapDefs (Vector a)) where
--   (MapDefs im) <> (MapDefs im') = MapDefs (im `M.union` M.mapKeys (+ leftMaxId) im')
--     where
--       leftMaxId = M.foldlWithKey' (\a i _ -> max a i) 1 im

-- parse :: (HasCallStack) => (Int, Int) -> MapData -> Either String (Vector (BB.Box V2 Int, MapDef, ObjectId))
-- parse (x, y) (Fill def) = Right $ V.singleton (BB.Box (V2 0 0) (V2 x y), def, ObjectId 0)
-- parse size (Tips {..}) = do
--   b <- foldMapM (parseMap size) maps
--   V.imapM
--     ( \i (bb, k) -> do
--         def <- maybeToRight ("key: " ++ show k ++ " not found") (defs M.!? k)
--         return (bb, def, ObjectId i)
--     )
--     b

-- parseMapDef :: (HasCallStack) => (Int, Int) -> MapData -> Either String (Vector (BB.Box V2 Int, MapDef, ObjectId), MapDefs (Vector (V2 Int, ObjectId)))
-- parseMapDef size md = do
--   ls <- parse size md
--   return
--     ( ls,
--       MapDefs
--         ( V.foldl'
--             ( \im (BB.Box start end, def, objId) ->
--                 let V2 w h = end - start
--                     size = (w, h)
--                  in M.alter (Just . maybe (def, size, V.singleton (start, objId)) (third3 (`V.snoc` (start, objId)))) objId im
--             )
--             M.empty
--             ls
--         )
--     )

adjust :: Int -> Bool -> Float -> Vector (Int, Int) -> V.Vector (Int, Float)
adjust sz asc crossSize v =
  if
    | null s -> (0, e0) `V.cons` v' `V.snoc` (sz, en)
    | (fst (V.head s) == 0) && (fst (V.last s) == sz) -> v'
    | (fst (V.head s) /= 0) && (fst (V.last s) == sz) -> (0, e0) `V.cons` s'
    | (fst (V.head s) == 0) && (fst (V.last s) /= sz) -> s' `V.snoc` (sz, en)
    | otherwise -> (0, e0) `V.cons` s' `V.snoc` (sz, en)
  where
    s = V.modify (VAI.sortBy (comparing fst)) (V.filter ((\a -> a >= 0 && a <= sz) . fst) v)
    e0 = if asc then 0 else crossSize
    en = if asc then crossSize else 0
    v' = V.map (second fromIntegral) v
    s' = V.map (second fromIntegral) s

splined :: (HasCallStack) => SplinePair Float -> V2 Float -> V2 Float
splined (xsp, ysp) (V2 x y) = V2 x rx + V2 ry y
  where
    rx = calcSplines xsp x
    ry = calcSplines ysp y

splined3 :: (HasCallStack) => SplinePair Float -> V3 Float -> V3 Float
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
