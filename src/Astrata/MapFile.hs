{-# LANGUAGE DerivingVia #-}

module Astrata.MapFile where

import Astrata.Spline
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Linear.V2
import Linear.V3
import RIO

type EdgePoint = (Bool, Bool)

type Color = (Float, Float, Float, Float)

type RGBA = (Int, Int, Int, Int)

rgba :: RGBA -> Color
rgba (r, g, b, a) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, fromIntegral a / 255)

data FaceId
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
  deriving (Show, Eq, Ord)

newtype CardInstanceId = CardInstanceId Int
  deriving (Show, Eq, Ord)
  deriving (Num) via Int

data CardAction
  = Draw Int
  | Discard Int
  deriving (Show, Eq)

data ActionCost
  = DiscardHand Int
  | DiscardAura Int

data OriginLevel = L0 | L1 | L2 | L3
  deriving (Show, Eq)

type Optionality = Bool

data BattlerStatus
  = Normal
  | Tired
  deriving (Show, Eq)

data Phase
  = StartPhase
  | EvolPhase
  | ActionPhase
  | AttackPhase
  | EndPhase
  deriving (Show)

data Battler = Battler
  { atk :: Int,
    def :: Int,
    status :: BattlerStatus,
    effects :: [(Phase, [CardAction], Optionality)],
    faceId :: FaceId,
    instanceId :: CardInstanceId
  }
  deriving (Show)

data OriginSlot = OriginSlot
  { level :: OriginLevel,
    hp :: Int,
    status :: BattlerStatus,
    planet :: Planet,
    aura :: [Battler],
    instanceId :: CardInstanceId
  }
  deriving (Show)

data SatelliteSlot = EmptySlot | SealedSlot | Assigned Battler
  deriving (Show)

data Planet = Mercury
  deriving (Show, Eq)

battler :: SatelliteSlot -> Maybe Battler
battler (Assigned b) = Just b
battler _ = Nothing

data FieldSide = Own | Opponent
  deriving (Eq, Ord, Show)

originFaceId :: OriginLevel -> FaceId
originFaceId L0 = Origin0
originFaceId L1 = Origin1
originFaceId L2 = Origin2
originFaceId L3 = Origin3

originToBattler :: OriginSlot -> Battler
originToBattler slot =
  Battler
    { atk = 1,
      def = 1,
      faceId = originFaceId slot.level,
      status = slot.status,
      effects = [(StartPhase, [], True)],
      instanceId = slot.instanceId
    }

data Field = Field
  { origin :: OriginSlot,
    frontL :: SatelliteSlot,
    frontR :: SatelliteSlot,
    backL :: SatelliteSlot,
    backR :: SatelliteSlot,
    astraRecords :: V.Vector Battler
  }
  deriving (Show)

data FieldSlot
  = Origin
  | FrontL
  | FrontR
  | BackL
  | BackR
  | AstraRecords
  | Heap
  deriving (Show, Eq)

data CardSelection = CardSelection
  { selectedField :: Maybe FieldSide,
    selectedSlot :: Maybe FieldSlot,
    selectedHandIndex :: Maybe Int
  }
  deriving (Show, Eq)

initialCardSelection :: CardSelection
initialCardSelection = CardSelection Nothing Nothing Nothing

selectHandCard :: Int -> BattleField -> FieldSide -> CardSelection
selectHandCard index _bf side = CardSelection (Just side) Nothing (Just index)

selectFieldSlot :: FieldSlot -> FieldSide -> CardSelection
selectFieldSlot slot side = CardSelection (Just side) (Just slot) Nothing

getSelectedCard :: CardSelection -> BattleField -> Maybe FaceId
getSelectedCard (CardSelection (Just side) Nothing (Just handIndex)) bf =
  case side of
    Own -> bf.ownHands V.!? handIndex
    Opponent -> bf.rivalHands V.!? handIndex
getSelectedCard _ _ = Nothing

getSelectedBattler :: CardSelection -> BattleField -> Maybe Battler
getSelectedBattler (CardSelection (Just side) (Just slot) Nothing) bf =
  let field = case side of
        Own -> bf.own
        Opponent -> bf.rival
   in case slot of
        Origin -> Just $ originToBattler field.origin
        FrontL -> battler field.frontL
        FrontR -> battler field.frontR
        BackL -> battler field.backL
        BackR -> battler field.backR
        AstraRecords -> Nothing
        Heap -> Nothing
getSelectedBattler _ _ = Nothing

data BattleField = BattleField
  { own :: Field,
    rival :: Field,
    ownHands :: V.Vector FaceId,
    rivalHands :: V.Vector FaceId
  }
  deriving (Show)

-- Initial battlefield for testing
initialBattleField :: BattleField
initialBattleField =
  BattleField
    { own =
        Field
          { origin = OriginSlot {level = L0, hp = 6, status = Normal, planet = Mercury, aura = [], instanceId = 0},
            frontL = Assigned Battler {atk = 1, def = 1, status = Normal, effects = [], faceId = S7, instanceId = 1},
            frontR = Assigned Battler {atk = 1, def = 1, status = Tired, effects = [], faceId = S7, instanceId = 2},
            backL = Assigned Battler {atk = 1, def = 1, status = Normal, effects = [], faceId = S7, instanceId = 3},
            backR = Assigned Battler {atk = 1, def = 1, status = Tired, effects = [], faceId = S7, instanceId = 4},
            astraRecords = V.empty
          },
      rival =
        Field
          { origin = OriginSlot {level = L0, hp = 6, status = Normal, planet = Mercury, aura = [], instanceId = 5},
            frontL = Assigned Battler {atk = 1, def = 1, status = Normal, effects = [], faceId = S7, instanceId = 6},
            frontR = Assigned Battler {atk = 1, def = 1, status = Tired, effects = [], faceId = S7, instanceId = 7},
            backL = Assigned Battler {atk = 1, def = 1, status = Tired, effects = [], faceId = S7, instanceId = 8},
            backR = EmptySlot,
            astraRecords = V.empty
          },
      ownHands = V.empty,
      rivalHands = V.empty
    }

data ShapeDef
  = Empty
  | Card
      { height :: Float,
        yOffset :: Float,
        cardFaceId :: Maybe FaceId
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
  | Sphere
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Gltf FilePath
  | Glb FilePath
  deriving (Show, Eq, Ord)

vectorToMaybe :: Vector a -> Maybe a
vectorToMaybe = V.foldr (const . Just) Nothing
{-# INLINE vectorToMaybe #-}

type Size a = (a, a)

data Axis = X | Y
  deriving (Eq, Ord, Show)

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
