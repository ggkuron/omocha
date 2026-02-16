{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Astrata.MapFile where

import Astrata.Bitmap
  ( Bitmap,
    loadBitmapsWith,
  )
import Data.Vector qualified as V
import GHC.Enum (pred, succ)
import Paths_astrata (getDataFileName)
import RIO
import Text.Show (Show (showsPrec), showString, shows)

loadBitmapsWith [|getDataFileName|] "../../static/images"

next :: (Bounded a, Enum a, Eq a) => a -> a
next x =
  if x == maxBound then minBound else succ x

prev :: (Bounded a, Enum a, Eq a) => a -> a
prev x =
  if x == minBound then maxBound else pred x

type Color = (Float, Float, Float, Float)

type RGBA = (Int, Int, Int, Int)

rgba :: RGBA -> Color
rgba (r, g, b, a) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, fromIntegral a / 255)

data CardPlanet = Moon | Mercury
  deriving (Show, Eq)

data GeneralProp = GeneralProp
  { level :: Level,
    name :: Text,
    action :: [CardAction],
    bitmap :: Bitmap
  }

instance Eq GeneralProp where
  a == b = a.name == b.name

instance Show GeneralProp where
  showsPrec _ (GeneralProp lvl nm acts _bmp) =
    showString "GeneralProp {level = "
      . shows lvl
      . showString ", name = "
      . shows nm
      . showString ", action = "
      . shows acts
      . showString "}"

data BattlerProp = BattlerProp
  { atk :: Int,
    def :: Int
  }
  deriving (Eq, Show)

newtype GuardProp = GuardProp
  { def :: Int
  }
  deriving (Eq, Show)

data Satelite deriving (Show, Eq)

data Origin deriving (Show, Eq)

data Portion deriving (Show, Eq)

data Arts deriving (Show, Eq)

data AsGuard deriving (Show, Eq)

data Card s where
  Satelite :: GeneralProp -> BattlerProp -> Card Satelite
  Origin :: GeneralProp -> BattlerProp -> Card Origin
  Portion :: GeneralProp -> Card Portion
  Arts :: GeneralProp -> Card Arts
  AsGuard :: GeneralProp -> GuardProp -> Card AsGuard

deriving instance Show (Card s)

class CardClass s where
  cardProp :: s -> GeneralProp

deriving instance Eq (Card s)

instance CardClass (Card s) where
  cardProp (Satelite gp _) = gp
  cardProp (Origin gp _) = gp
  cardProp (Portion gp) = gp
  cardProp (Arts gp) = gp
  cardProp (AsGuard gp _) = gp

cardBitmap :: (CardClass a) => a -> Bitmap
cardBitmap = (.bitmap) . cardProp

data MainCard = MainSatellite (Card Satelite) | MainPortion (Card Portion) | MainArts (Card Arts)
  deriving (Show, Eq)

instance CardClass MainCard where
  cardProp (MainSatellite c) = cardProp c
  cardProp (MainPortion c) = cardProp c
  cardProp (MainArts c) = cardProp c

data Deck
  = Deck
  { main :: V.Vector MainCard,
    guard :: V.Vector (Card AsGuard),
    origin :: V.Vector (Card Origin)
  }

newtype CardInstanceId = CardInstanceId Int
  deriving (Show, Eq, Ord)
  deriving (Num) via Int

data ActionTarget
  = SelfTarget
  | AllTargets
  | SelfOrigin
  | SelfSatteliteSelect Int
  deriving (Show, Eq)

data CardEffect
  = Draw Int
  | Discard Int
  | GainAtk Int
  deriving (Show, Eq)

data ActionCost
  = DiscardHand Int
  | UseAura Int
  deriving (Show, Eq)

data Level = L0 | L1 | L2 | L3
  deriving (Show, Eq, Ord)

type Optionality = Bool

data CardAction = CardAction
  { phase :: Phase,
    costs :: [ActionCost],
    effects :: [(CardEffect, ActionTarget, Optionality)]
  }
  deriving (Show, Eq)

data Phase
  = StartPhase
  | EvolPhase
  | ActionPhase
  | AttackPhase
  | EndPhase
  deriving (Show, Eq, Ord)

data OpenStatus = Hidden | Private | Revealed
  deriving (Show, Eq)

type Active = Bool

data CardInstance s where
  SateliteInstance :: Card Satelite -> CardInstanceId -> BattleInstance -> CardInstance Satelite
  OriginInstance :: Card Origin -> CardInstanceId -> BattleInstance -> OriginStatus -> CardInstance Origin
  PortionInstance :: Card Portion -> CardInstanceId -> CardInstance Portion
  ArtsInstance :: Card Arts -> CardInstanceId -> CardInstance Arts
  AsGuardInstance :: Card AsGuard -> CardInstanceId -> CardInstance AsGuard

deriving instance Show (CardInstance s)

class CardInstanceClass s where
  instanceId :: s -> CardInstanceId

instance CardClass (CardInstance s) where
  cardProp (SateliteInstance c _ _) = cardProp c
  cardProp (OriginInstance c _ _ _) = cardProp c
  cardProp (PortionInstance c _) = cardProp c
  cardProp (ArtsInstance c _) = cardProp c
  cardProp (AsGuardInstance c _) = cardProp c

instance CardInstanceClass (CardInstance s) where
  instanceId (SateliteInstance _ id _) = id
  instanceId (OriginInstance _ id _ _) = id
  instanceId (PortionInstance _ id) = id
  instanceId (ArtsInstance _ id) = id
  instanceId (AsGuardInstance _ id) = id

data HandCardInstance where
  HandSatelliteInstance ::
    CardInstance Satelite ->
    HandCardInstance
  HandPortionInstance ::
    CardInstance Portion ->
    HandCardInstance
  HandArtsInstance ::
    CardInstance Arts ->
    HandCardInstance

data AllCardInstance where
  AllCardInstance :: forall s. (CardInstance s) -> AllCardInstance

instance CardInstanceClass HandCardInstance where
  instanceId (HandSatelliteInstance ci) = instanceId ci
  instanceId (HandPortionInstance ci) = instanceId ci
  instanceId (HandArtsInstance ci) = instanceId ci

isTired :: CardInstance s -> Bool
isTired (SateliteInstance _ _ battle) = battle.tired
isTired (OriginInstance _ _ battle _) = battle.tired
isTired _ = False

data BattleInstance = BattleInstance
  { tired :: Bool,
    effects :: [CardAction]
  }
  deriving (Show)

newtype OriginStatus = OriginStatus
  { hp :: Int
  }
  deriving (Show)

data SatelliteSlot = EmptySlot | SealedSlot | Assigned (CardInstance Satelite)
  deriving (Show)

maybeSlot :: SatelliteSlot -> Maybe (CardInstance Satelite)
maybeSlot (Assigned b) = Just b
maybeSlot _ = Nothing

data FieldSide = Own | Opponent
  deriving (Eq, Show, Enum, Bounded)

data Field = Field
  { origin :: CardInstance Origin,
    frontL :: SatelliteSlot,
    frontR :: SatelliteSlot,
    backL :: SatelliteSlot,
    backR :: SatelliteSlot,
    aura :: V.Vector HandCardInstance,
    astraRecords :: V.Vector AllCardInstance,
    hands :: V.Vector HandCardInstance
  }

data FieldZone = BattleZone | AuraZone | AstraRecordZone | HandZone | DeckZone | GuardZone
  deriving (Show, Eq, Enum, Bounded)

data Selection = SideSelect FieldSide | FiledSelect FieldSide FieldZone | BattleSlotSelect FieldSide FieldZone BattleFieldSlot | CardSelect CardInstanceId
  deriving (Show, Eq)

data BattleFieldSlot
  = OriginSlot
  | FrontL
  | FrontR
  | BackL
  | BackR
  deriving (Show, Eq, Enum, Bounded)

data BattleField = BattleField
  { own :: Field,
    rival :: Field
  }

cardM0 :: Card Origin
cardM0 = Origin (GeneralProp {level = L0, name = "Moon Origin0", action = [], bitmap = _S__23388166_0_jpg}) (BattlerProp {atk = 1, def = 1})

cardS0 :: Card Satelite
cardS0 = Satelite (GeneralProp {level = L0, name = "Moon Satellite0", action = [], bitmap = _S__23388170_0_jpg}) (BattlerProp {atk = 1, def = 1})

-- Initial battlefield for testing
initialBattleField :: BattleField
initialBattleField =
  BattleField
    { own =
        Field
          { origin = OriginInstance cardM0 0 (BattleInstance {tired = False, effects = []}) (OriginStatus {hp = 6}),
            frontL = Assigned $ SateliteInstance cardS0 1 $ BattleInstance {tired = False, effects = []},
            frontR = Assigned $ SateliteInstance cardS0 2 $ BattleInstance {tired = False, effects = []},
            backL = Assigned $ SateliteInstance cardS0 3 $ BattleInstance {tired = False, effects = []},
            backR = Assigned $ SateliteInstance cardS0 4 $ BattleInstance {tired = False, effects = []},
            astraRecords = V.empty,
            aura = V.empty,
            hands = V.empty
          },
      rival =
        Field
          { origin = OriginInstance cardM0 5 (BattleInstance {tired = False, effects = []}) (OriginStatus {hp = 6}),
            frontL = Assigned $ SateliteInstance cardS0 6 $ BattleInstance {tired = False, effects = []},
            frontR = Assigned $ SateliteInstance cardS0 7 $ BattleInstance {tired = False, effects = []},
            backL = Assigned $ SateliteInstance cardS0 8 $ BattleInstance {tired = False, effects = []},
            backR = EmptySlot,
            astraRecords = V.empty,
            aura = V.empty,
            hands = V.empty
          }
    }

type Size a = (a, a)
