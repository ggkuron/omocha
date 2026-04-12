let card_satellite_s0 : SatelliteCard = {
  prop: { name: "Satellite-S0", cost: 1 },
  battler: { atk: 1, hp: 2 },
}
let card_satellite_m0 : SatelliteCard = {
  prop: { name: "Satellite-M0", cost: 2 },
  battler: { atk: 2, hp: 3 },
}
let card_origin_base : OriginCard = {
  prop: { name: "Origin", cost: 0 },
  max_hp: 6,
}
fn make_origin(id : CardInstanceId) -> OriginInstance {
  { id, card: card_origin_base, state: { current_hp: card_origin_base.max_hp } }
}
fn make_satellite(
  id : CardInstanceId,
  card : SatelliteCard,
) -> SatelliteInstance {
  { id, card, battle: { tired: false, current_hp: card.battler.hp } }
}
pub let initial_battle_field : BattleField = {
  own: make_own_field(),
  rival: make_rival_field(),
}
pub fn initial_game_state() -> GameState {
  { battlefield: initial_battle_field, turn: Own, phase: StartPhase, log: [] }
}
fn make_own_field() -> Field {
  let hands : Array[HandCard] = [
    HSatellite(make_satellite(101, card_satellite_s0)),
    HSatellite(make_satellite(102, card_satellite_s0)),
    HSatellite(make_satellite(103, card_satellite_s0)),
    HSatellite(make_satellite(104, card_satellite_m0)),
    HSatellite(make_satellite(105, card_satellite_m0)),
  ]
  {
    origin: make_origin(100),
    front_l: Empty,
    front_r: Empty,
    back_l: Empty,
    back_r: Empty,
    aura: [],
    astra_records: [],
    hands,
    stack: [],
  }
}
fn make_rival_field() -> Field {
  let hands : Array[HandCard] = [
    HSatellite(make_satellite(201, card_satellite_s0)),
    HSatellite(make_satellite(202, card_satellite_s0)),
    HSatellite(make_satellite(203, card_satellite_m0)),
    HSatellite(make_satellite(204, card_satellite_m0)),
    HSatellite(make_satellite(205, card_satellite_s0)),
  ]
  {
    origin: make_origin(200),
    front_l: Assigned(make_satellite(206, card_satellite_s0)),
    front_r: Assigned(make_satellite(207, card_satellite_s0)),
    back_l: Assigned(make_satellite(208, card_satellite_m0)),
    back_r: Empty,
    aura: [],
    astra_records: [],
    hands,
    stack: [],
  }
}
