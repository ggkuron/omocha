///|
fn make_all_slots_assigned_own_field() -> Field {
  let base = initial_battle_field.own
  let s1 = make_satellite(111, card_satellite_s0)
  let s2 = make_satellite(112, card_satellite_s0)
  let s3 = make_satellite(113, card_satellite_m0)
  let s4 = make_satellite(114, card_satellite_m0)
  {
    origin: base.origin,
    front_l: Assigned(s1),
    front_r: Assigned(s2),
    back_l: Assigned(s3),
    back_r: Assigned(s4),
    aura: base.aura,
    astra_records: base.astra_records,
    hands: base.hands,
    stack: base.stack,
  }
}

///|
fn make_only_frontl_empty_own_field() -> Field {
  let base = initial_battle_field.own
  let s1 = make_satellite(121, card_satellite_s0)
  let s2 = make_satellite(122, card_satellite_m0)
  let s3 = make_satellite(123, card_satellite_m0)
  {
    origin: base.origin,
    front_l: Empty,
    front_r: Assigned(s1),
    back_l: Assigned(s2),
    back_r: Assigned(s3),
    aura: base.aura,
    astra_records: base.astra_records,
    hands: base.hands,
    stack: base.stack,
  }
}

///|
fn make_frontl_frontr_assigned_own_field() -> Field {
  let base = initial_battle_field.own
  let s1 = make_satellite(131, card_satellite_s0)
  let s2 = make_satellite(132, card_satellite_s0)
  {
    origin: base.origin,
    front_l: Assigned(s1),
    front_r: Assigned(s2),
    back_l: Empty,
    back_r: Empty,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: base.hands,
    stack: base.stack,
  }
}

///|
fn make_only_backr_empty_own_field() -> Field {
  let base = initial_battle_field.own
  let s1 = make_satellite(141, card_satellite_s0)
  let s2 = make_satellite(142, card_satellite_s0)
  let s3 = make_satellite(143, card_satellite_m0)
  {
    origin: base.origin,
    front_l: Assigned(s1),
    front_r: Assigned(s2),
    back_l: Assigned(s3),
    back_r: Empty,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: base.hands,
    stack: base.stack,
  }
}

///|
fn make_empty_hands_own_field() -> Field {
  let base = initial_battle_field.own
  {
    origin: base.origin,
    front_l: base.front_l,
    front_r: base.front_r,
    back_l: base.back_l,
    back_r: base.back_r,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: [],
    stack: base.stack,
  }
}

///|
fn make_portion_first_hands_own_field() -> Field {
  let base = initial_battle_field.own
  let portion = HPortion({
    id: 151,
    card: { prop: { name: "Portion", cost: 1 }, effect: "heal" },
  })
  let new_hands : Array[HandCard] = [portion]
  for i in 0..<base.hands.length() {
    new_hands.push(base.hands[i])
  }
  {
    origin: base.origin,
    front_l: base.front_l,
    front_r: base.front_r,
    back_l: base.back_l,
    back_r: base.back_r,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: new_hands,
    stack: base.stack,
  }
}

///|
fn slot_instance_id(slot : SatelliteSlot) -> Int {
  match slot {
    Assigned(inst) => inst.id
    _ => -1
  }
}
