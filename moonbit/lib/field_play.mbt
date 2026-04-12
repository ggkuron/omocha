///|
pub fn play_card(field : Field) -> Field {
  if field.hands.length() == 0 {
    return field
  }
  match field.hands[0] {
    HSatellite(inst) => place_satellite_to_empty(field, inst)
    _ => field
  }
}

///|
pub fn apply_play_own_card(state : GameState) -> GameState {
  let new_own = play_card(state.battlefield.own)
  {
    battlefield: { own: new_own, rival: state.battlefield.rival },
    turn: state.turn,
    phase: state.phase,
    log: state.log,
  }
}

///|
fn is_empty_slot(slot : SatelliteSlot) -> Bool {
  match slot {
    Empty => true
    _ => false
  }
}

///|
fn place_satellite_to_empty(field : Field, inst : SatelliteInstance) -> Field {
  let new_hands : Array[HandCard] = []
  for i in 1..<field.hands.length() {
    new_hands.push(field.hands[i])
  }
  if is_empty_slot(field.front_l) {
    return assign_satellite(field, inst, new_hands, FrontL)
  }
  if is_empty_slot(field.front_r) {
    return assign_satellite(field, inst, new_hands, FrontR)
  }
  if is_empty_slot(field.back_l) {
    return assign_satellite(field, inst, new_hands, BackL)
  }
  if is_empty_slot(field.back_r) {
    return assign_satellite(field, inst, new_hands, BackR)
  }
  field
}

///|
fn assign_satellite(
  field : Field,
  inst : SatelliteInstance,
  new_hands : Array[HandCard],
  slot : BattleFieldSlot,
) -> Field {
  {
    origin: field.origin,
    front_l: slot_value(slot, FrontL, field.front_l, inst),
    front_r: slot_value(slot, FrontR, field.front_r, inst),
    back_l: slot_value(slot, BackL, field.back_l, inst),
    back_r: slot_value(slot, BackR, field.back_r, inst),
    aura: field.aura,
    astra_records: field.astra_records,
    hands: new_hands,
    stack: field.stack,
  }
}

///|
fn slot_value(
  selected : BattleFieldSlot,
  target : BattleFieldSlot,
  current : SatelliteSlot,
  inst : SatelliteInstance,
) -> SatelliteSlot {
  if selected == target {
    Assigned(inst)
  } else {
    current
  }
}
