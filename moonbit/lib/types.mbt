pub type CardInstanceId = Int
pub struct GeneralProp {
  name : String
  cost : Int
}
pub struct BattlerProp {
  atk : Int
  hp : Int
}
pub struct BattleState {
  tired : Bool
  current_hp : Int
}
pub struct SatelliteCard {
  prop : GeneralProp
  battler : BattlerProp
}
pub struct OriginCard {
  prop : GeneralProp
  max_hp : Int
}
pub struct PortionCard {
  prop : GeneralProp
  effect : String
}
pub struct ArtsCard {
  prop : GeneralProp
  effect : String
}
pub struct AsGuardCard {
  prop : GeneralProp
  effect : String
}
pub struct SatelliteInstance {
  id : CardInstanceId
  card : SatelliteCard
  battle : BattleState
}
pub struct OriginState {
  current_hp : Int
}
pub struct OriginInstance {
  id : CardInstanceId
  card : OriginCard
  state : OriginState
}
pub struct PortionInstance {
  id : CardInstanceId
  card : PortionCard
}
pub struct ArtsInstance {
  id : CardInstanceId
  card : ArtsCard
}
pub struct AsGuardInstance {
  id : CardInstanceId
  card : AsGuardCard
}
pub enum HandCard {
  HSatellite(SatelliteInstance)
  HPortion(PortionInstance)
  HGuard(AsGuardInstance)
}
pub enum AstraRecord {
  ARSatellite(SatelliteInstance)
  ARPortion(PortionInstance)
  ARArts(ArtsInstance)
  ARGuard(AsGuardInstance)
}
pub enum SatelliteSlot {
  Empty
  Sealed
  Assigned(SatelliteInstance)
}
pub struct Field {
  origin : OriginInstance
  front_l : SatelliteSlot
  front_r : SatelliteSlot
  back_l : SatelliteSlot
  back_r : SatelliteSlot
  aura : Array[HandCard]
  astra_records : Array[AstraRecord]
  hands : Array[HandCard]
  stack : Array[HandCard]
}
pub struct BattleField {
  own : Field
  rival : Field
}
pub(all) enum FieldSide {
  Own
  Rival
} derive(Show, Eq, Compare)
pub(all) enum FieldZone {
  BattleZone
  AuraZone
  GuardZone
} derive(Show, Eq, Compare)
pub(all) enum BattleFieldSlot {
  OriginSlot
  FrontL
  FrontR
  BackL
  BackR
} derive(Show, Eq, Compare)
pub(all) enum Selection {
  SideSelect(FieldSide)
  FieldSelect(FieldSide, FieldZone)
  SlotSelect(FieldSide, FieldZone, BattleFieldSlot)
} derive(Show, Eq)
pub enum Phase {
  StartPhase
  EvolPhase
  ActionPhase
  AttackPhase
  EndPhase
} derive(Show, Eq)
pub struct GameState {
  battlefield : BattleField
  turn : FieldSide
  phase : Phase
  log : Array[String]
}
