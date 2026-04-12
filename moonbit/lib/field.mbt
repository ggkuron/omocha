pub fn draw_one_card(field : Field) -> Field {
  if field.stack.length() == 0 {
    return field
  }
  let card = field.stack[0]
  let new_stack : Array[HandCard] = []
  for i in 1..<field.stack.length() {
    new_stack.push(field.stack[i])
  }
  let new_hands = field.hands.copy()
  new_hands.push(card)
  {
    origin: field.origin,
    front_l: field.front_l,
    front_r: field.front_r,
    back_l: field.back_l,
    back_r: field.back_r,
    aura: field.aura,
    astra_records: field.astra_records,
    hands: new_hands,
    stack: new_stack,
  }
}
pub fn is_assigned(field : Field, slot : BattleFieldSlot) -> Bool {
  match slot {
    OriginSlot => true
    FrontL => is_satellite_assigned(field.front_l)
    FrontR => is_satellite_assigned(field.front_r)
    BackL => is_satellite_assigned(field.back_l)
    BackR => is_satellite_assigned(field.back_r)
  }
}
fn is_satellite_assigned(slot : SatelliteSlot) -> Bool {
  match slot {
    Assigned(_) => true
    _ => false
  }
}
pub fn next_selection(sel : Selection) -> Selection {
  match sel {
    SideSelect(side) => FieldSelect(side, BattleZone)
    FieldSelect(side, BattleZone) => SlotSelect(side, BattleZone, OriginSlot)
    FieldSelect(_, AuraZone | GuardZone) => sel
    SlotSelect(_, _, _) => sel
  }
}
pub fn prev_selection(sel : Selection) -> Selection {
  match sel {
    SideSelect(_) => sel
    FieldSelect(side, _) => SideSelect(side)
    SlotSelect(side, zone, _) => FieldSelect(side, zone)
  }
}
pub fn next_in_level(bf : BattleField, sel : Selection) -> Selection {
  match sel {
    SideSelect(side) => SideSelect(next_side(side))
    FieldSelect(side, zone) => FieldSelect(side, next_zone(zone))
    SlotSelect(side, zone, slot) =>
      next_assigned_slot(bf, side, zone, slot, next_slot)
  }
}
pub fn prev_in_level(bf : BattleField, sel : Selection) -> Selection {
  match sel {
    SideSelect(side) => SideSelect(prev_side(side))
    FieldSelect(side, zone) => FieldSelect(side, prev_zone(zone))
    SlotSelect(side, zone, slot) =>
      next_assigned_slot(bf, side, zone, slot, prev_slot)
  }
}
fn next_side(side : FieldSide) -> FieldSide {
  match side {
    Own => Rival
    Rival => Own
  }
}
fn prev_side(side : FieldSide) -> FieldSide {
  next_side(side)
}
fn next_zone(zone : FieldZone) -> FieldZone {
  match zone {
    BattleZone => AuraZone
    AuraZone => GuardZone
    GuardZone => BattleZone
  }
}
fn prev_zone(zone : FieldZone) -> FieldZone {
  match zone {
    BattleZone => GuardZone
    AuraZone => BattleZone
    GuardZone => AuraZone
  }
}
fn next_slot(slot : BattleFieldSlot) -> BattleFieldSlot {
  match slot {
    OriginSlot => FrontL
    FrontL => FrontR
    FrontR => BackL
    BackL => BackR
    BackR => OriginSlot
  }
}
fn prev_slot(slot : BattleFieldSlot) -> BattleFieldSlot {
  match slot {
    OriginSlot => BackR
    FrontL => OriginSlot
    FrontR => FrontL
    BackL => FrontR
    BackR => BackL
  }
}
fn next_assigned_slot(
  bf : BattleField,
  side : FieldSide,
  zone : FieldZone,
  current : BattleFieldSlot,
  step_fn : (BattleFieldSlot) -> BattleFieldSlot,
) -> Selection {
  let field = field_for(bf, side)
  let candidate = step_fn(current)
  find_assigned_from(field, side, zone, candidate, step_fn)
}
fn find_assigned_from(
  field : Field,
  side : FieldSide,
  zone : FieldZone,
  slot : BattleFieldSlot,
  step_fn : (BattleFieldSlot) -> BattleFieldSlot,
) -> Selection {
  if is_assigned(field, slot) {
    SlotSelect(side, zone, slot)
  } else {
    find_assigned_from(field, side, zone, step_fn(slot), step_fn)
  }
}
pub fn field_for(bf : BattleField, side : FieldSide) -> Field {
  match side {
    Own => bf.own
    Rival => bf.rival
  }
}
