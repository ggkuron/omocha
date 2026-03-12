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

data MainCard = MainSatellite (Card Satelite) | MainPortion (Card Portion) | MainGuard (Card AsGuard)
  deriving (Show, Eq)

instance CardClass MainCard where
  cardProp (MainSatellite c) = cardProp c
  cardProp (MainPortion c) = cardProp c
  cardProp (MainGuard c) = cardProp c

data Deck
  = Deck
  { main :: V.Vector MainCard,
    arts :: V.Vector (Card Arts),
    origin :: V.Vector (Card Origin)
  }

newtype CardInstanceId = CardInstanceId Int
  deriving (Show, Eq, Ord, Enum)
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
  SateliteInstance :: Card Satelite -> CardInstanceId -> BattleInstance -> OpenStatus -> CardInstance Satelite
  OriginInstance :: Card Origin -> CardInstanceId -> BattleInstance -> OriginStatus -> OpenStatus -> CardInstance Origin
  PortionInstance :: Card Portion -> CardInstanceId -> OpenStatus -> CardInstance Portion
  ArtsInstance :: Card Arts -> CardInstanceId -> OpenStatus -> CardInstance Arts
  AsGuardInstance :: Card AsGuard -> CardInstanceId -> OpenStatus -> CardInstance AsGuard

deriving instance Show (CardInstance s)

class CardInstanceClass s where
  instanceId :: s -> CardInstanceId

instance CardClass (CardInstance s) where
  cardProp (SateliteInstance c _ _ _) = cardProp c
  cardProp (OriginInstance c _ _ _ _) = cardProp c
  cardProp (PortionInstance c _ _) = cardProp c
  cardProp (ArtsInstance c _ _) = cardProp c
  cardProp (AsGuardInstance c _ _) = cardProp c

instance CardInstanceClass (CardInstance s) where
  instanceId (SateliteInstance _ id _ _) = id
  instanceId (OriginInstance _ id _ _ _) = id
  instanceId (PortionInstance _ id _) = id
  instanceId (ArtsInstance _ id _) = id
  instanceId (AsGuardInstance _ id _) = id

data HandCardInstance where
  HandSatelliteInstance ::
    CardInstance Satelite ->
    HandCardInstance
  HandPortionInstance ::
    CardInstance Portion ->
    HandCardInstance
  HandGuardInstance ::
    CardInstance AsGuard ->
    HandCardInstance
  deriving (Show)

data AllCardInstance where
  AllSatelliteInstance :: CardInstance Satelite -> AllCardInstance
  AllPortionInstance :: CardInstance Portion -> AllCardInstance
  AllArtsInstance :: CardInstance Arts -> AllCardInstance
  AllGuardInstance :: CardInstance AsGuard -> AllCardInstance
  deriving (Show)

instance CardInstanceClass HandCardInstance where
  instanceId (HandSatelliteInstance ci) = instanceId ci
  instanceId (HandPortionInstance ci) = instanceId ci
  instanceId (HandGuardInstance ci) = instanceId ci

instance CardClass HandCardInstance where
  cardProp (HandSatelliteInstance ci) = cardProp ci
  cardProp (HandPortionInstance ci) = cardProp ci
  cardProp (HandGuardInstance ci) = cardProp ci

isTired :: CardInstance s -> Bool
isTired (SateliteInstance _ _ battle _) = battle.tired
isTired (OriginInstance _ _ battle _ _) = battle.tired
isTired _ = False

openStatus :: CardInstance s -> OpenStatus
openStatus (SateliteInstance _ _ _ s) = s
openStatus (OriginInstance _ _ _ _ s) = s
openStatus (PortionInstance _ _ s) = s
openStatus (ArtsInstance _ _ s) = s
openStatus (AsGuardInstance _ _ s) = s

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
    arts :: V.Vector (CardInstance Arts),
    hands :: V.Vector HandCardInstance,
    stack :: V.Vector HandCardInstance
  }
  deriving (Show)

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
  deriving (Show)

-- | サイドに対応する Field を取得
fieldFor :: BattleField -> FieldSide -> Field
fieldFor bf Own = bf.own
fieldFor bf Opponent = bf.rival

-- | スロットが選択可能か（Assigned または OriginSlot）
isAssigned :: Field -> BattleFieldSlot -> Bool
isAssigned _ OriginSlot = True
isAssigned field FrontL = isJust (maybeSlot field.frontL)
isAssigned field FrontR = isJust (maybeSlot field.frontR)
isAssigned field BackL = isJust (maybeSlot field.backL)
isAssigned field BackR = isJust (maybeSlot field.backR)

-- | AllCardInstance の ID を取得
allCardInstanceId :: AllCardInstance -> CardInstanceId
allCardInstanceId (AllSatelliteInstance ci) = instanceId ci
allCardInstanceId (AllPortionInstance ci) = instanceId ci
allCardInstanceId (AllArtsInstance ci) = instanceId ci
allCardInstanceId (AllGuardInstance ci) = instanceId ci

-- | ゾーン内のカード ID 一覧を取得
zoneCardIds :: Field -> FieldZone -> V.Vector CardInstanceId
zoneCardIds field BattleZone =
  instanceId field.origin
    `V.cons` V.mapMaybe
      (fmap instanceId . maybeSlot)
      (V.fromList [field.frontL, field.frontR, field.backL, field.backR])
zoneCardIds field AuraZone = V.map instanceId field.aura
zoneCardIds field AstraRecordZone = V.map allCardInstanceId field.astraRecords
zoneCardIds field HandZone = V.map instanceId field.hands
zoneCardIds field DeckZone = V.map instanceId field.stack
zoneCardIds _ _ = V.empty

-- | カード ID がどのサイド・ゾーンにあるかを探す
findCardLocation :: BattleField -> CardInstanceId -> Maybe (FieldSide, FieldZone)
findCardLocation bf cid =
  let sides = V.fromList [minBound .. maxBound :: FieldSide]
      zones = V.fromList [minBound .. maxBound :: FieldZone]
      pairs = V.concatMap (\s -> V.map (s,) zones) sides
   in V.find (\(side, zone) -> V.elem cid (zoneCardIds (fieldFor bf side) zone)) pairs

-- | 次のスロットへ（EmptySlot/SealedSlot も含む）
nextAssignedSlot :: Field -> BattleFieldSlot -> BattleFieldSlot
nextAssignedSlot _field = next

-- | 前のスロットへ（EmptySlot/SealedSlot も含む）
prevAssignedSlot :: Field -> BattleFieldSlot -> BattleFieldSlot
prevAssignedSlot _field = prev

-- | 同ゾーン内の次カード ID へ
nextCardIdInZone :: BattleField -> CardInstanceId -> CardInstanceId
nextCardIdInZone bf cid = fromMaybe cid $ do
  (side, zone) <- findCardLocation bf cid
  let ids = zoneCardIds (fieldFor bf side) zone
  idx <- V.findIndex (== cid) ids
  ids V.!? (idx + 1)

-- | 同ゾーン内の前カード ID へ
prevCardIdInZone :: BattleField -> CardInstanceId -> CardInstanceId
prevCardIdInZone bf cid = fromMaybe cid $ do
  (side, zone) <- findCardLocation bf cid
  let ids = zoneCardIds (fieldFor bf side) zone
  idx <- V.findIndex (== cid) ids
  if idx > 0 then Just (ids V.! (idx - 1)) else Nothing

-- | 右キー: 一段深く入る（SideSelect→FiledSelect→BattleSlotSelect）
nextSelection :: BattleField -> Selection -> Selection
nextSelection _ (SideSelect side) = FiledSelect side minBound
nextSelection _ (FiledSelect side zone) = BattleSlotSelect side zone OriginSlot
nextSelection _ s = s

-- | 左キー: 一段上に戻る（BattleSlotSelect→FiledSelect→SideSelect）
prevSelection :: BattleField -> Selection -> Selection
prevSelection _ (BattleSlotSelect side zone _) = FiledSelect side zone
prevSelection _ (FiledSelect side _) = SideSelect side
prevSelection _ s = s

-- | 下キー: 同一レベル内で次へ
nextInLevel :: BattleField -> Selection -> Selection
nextInLevel _ (SideSelect side) = SideSelect (next side)
nextInLevel _ (FiledSelect side zone) = FiledSelect side (next zone)
nextInLevel bf (BattleSlotSelect side zone slot) =
  BattleSlotSelect side zone (nextAssignedSlot (fieldFor bf side) slot)
nextInLevel bf (CardSelect cid) = CardSelect (nextCardIdInZone bf cid)

-- | 上キー: 同一レベル内で前へ
prevInLevel :: BattleField -> Selection -> Selection
prevInLevel _ (SideSelect side) = SideSelect (prev side)
prevInLevel _ (FiledSelect side zone) = FiledSelect side (prev zone)
prevInLevel bf (BattleSlotSelect side zone slot) =
  BattleSlotSelect side zone (prevAssignedSlot (fieldFor bf side) slot)
prevInLevel bf (CardSelect cid) = CardSelect (prevCardIdInZone bf cid)

cardM0 :: Card Origin
cardM0 = Origin (GeneralProp {level = L0, name = "Moon Origin0", action = [], bitmap = _S__23388166_0_jpg}) (BattlerProp {atk = 1, def = 1})

cardS0 :: Card Satelite
cardS0 = Satelite (GeneralProp {level = L0, name = "Moon Satellite0", action = [], bitmap = _S__23388170_0_jpg}) (BattlerProp {atk = 1, def = 1})

mainCardToHandInstance :: MainCard -> CardInstanceId -> OpenStatus -> HandCardInstance
mainCardToHandInstance (MainSatellite c) cid os =
  HandSatelliteInstance (SateliteInstance c cid (BattleInstance {tired = False, effects = []}) os)
mainCardToHandInstance (MainPortion c) cid os =
  HandPortionInstance (PortionInstance c cid os)
mainCardToHandInstance (MainGuard c) cid os =
  HandGuardInstance (AsGuardInstance c cid os)

mkBattleField :: Deck -> Deck -> BattleField
mkBattleField ownDeck rivalDeck =
  let defaultBI = BattleInstance {tired = False, effects = []}
      defaultOS = OriginStatus {hp = 6}

      ownOrigin = OriginInstance (V.head ownDeck.origin) 0 defaultBI defaultOS Revealed
      ownDeck' = V.imap (\i c -> mainCardToHandInstance c (CardInstanceId (i + 1)) Hidden) ownDeck.main

      rivalStart = CardInstanceId (V.length ownDeck.main + 1)
      rivalOrigin = OriginInstance (V.head rivalDeck.origin) rivalStart defaultBI defaultOS Revealed
      rivalDeck' = V.imap (\i c -> mainCardToHandInstance c (rivalStart + CardInstanceId (i + 1)) Hidden) rivalDeck.main

      ownArtsStart = rivalStart + CardInstanceId (V.length rivalDeck.main + 1)
      ownArts = V.imap (\i c -> ArtsInstance c (ownArtsStart + CardInstanceId i) Private) ownDeck.arts

      rivalArtsStart = ownArtsStart + CardInstanceId (V.length ownDeck.arts)
      rivalArts = V.imap (\i c -> ArtsInstance c (rivalArtsStart + CardInstanceId i) Private) rivalDeck.arts

      mkField org dk ats =
        let (drawn, rest) = V.splitAt 5 dk
         in Field
              { origin = org,
                frontL = EmptySlot,
                frontR = EmptySlot,
                backL = EmptySlot,
                backR = EmptySlot,
                aura = V.empty,
                astraRecords = V.empty,
                arts = ats,
                hands = drawn,
                stack = rest
              }
   in BattleField {own = mkField ownOrigin ownDeck' ownArts, rival = mkField rivalOrigin rivalDeck' rivalArts}

ownDeck :: Deck
ownDeck =
  Deck
    { main = V.fromList [MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0],
      arts = V.empty,
      origin = V.fromList [cardM0]
    }

rivalDeck :: Deck
rivalDeck =
  Deck
    { main = V.fromList [MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0, MainSatellite cardS0],
      arts = V.empty,
      origin = V.fromList [cardM0]
    }

bf :: BattleField
bf = mkBattleField ownDeck rivalDeck

-- Initial battlefield for testing
initialBattleField :: BattleField
initialBattleField =
  BattleField
    { own = bf.own,
      rival =
        Field
          { origin = OriginInstance cardM0 6 (BattleInstance {tired = False, effects = []}) (OriginStatus {hp = 6}) Revealed,
            frontL = Assigned (SateliteInstance cardS0 7 (BattleInstance {tired = False, effects = []}) Revealed),
            frontR = Assigned (SateliteInstance cardS0 8 (BattleInstance {tired = False, effects = []}) Revealed),
            backL = Assigned (SateliteInstance cardS0 9 (BattleInstance {tired = False, effects = []}) Revealed),
            backR = EmptySlot,
            astraRecords = V.empty,
            arts = V.empty,
            aura = V.empty,
            hands = V.empty,
            stack = V.empty
          }
    }

type Size a = (a, a)
