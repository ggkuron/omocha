///|
test "play_card: 全スロット Assigned のとき Field は変化しない" {
  let field = make_all_slots_assigned_own_field()
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(result.hands.length(), hands_before)
  assert_eq(slot_instance_id(result.front_l), slot_instance_id(field.front_l))
  assert_eq(slot_instance_id(result.front_r), slot_instance_id(field.front_r))
  assert_eq(slot_instance_id(result.back_l), slot_instance_id(field.back_l))
  assert_eq(slot_instance_id(result.back_r), slot_instance_id(field.back_r))
}

///|
test "play_card: hands が空のとき Field は変化しない" {
  let field = make_empty_hands_own_field()
  let result = play_card(field)
  assert_eq(result.hands.length(), 0)
  assert_eq(is_assigned(result, FrontL), false)
  assert_eq(is_assigned(result, FrontR), false)
  assert_eq(is_assigned(result, BackL), false)
  assert_eq(is_assigned(result, BackR), false)
}

///|
test "play_card: hands[0] が Satellite でない場合 Field は変化しない" {
  let field = make_portion_first_hands_own_field()
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(result.hands.length(), hands_before)
  assert_eq(is_assigned(result, FrontL), false)
  assert_eq(is_assigned(result, FrontR), false)
  assert_eq(is_assigned(result, BackL), false)
  assert_eq(is_assigned(result, BackR), false)
}

///|
test "play_card: 配置後 hands[0] が除去され残りの手札が保持される" {
  let field = initial_battle_field.own
  let result = play_card(field)
  assert_eq(result.hands.length(), field.hands.length() - 1)
  match result.hands[0] {
    HSatellite(inst) => assert_eq(inst.id, 102)
    _ => abort("hands[0] は HSatellite であるべき")
  }
}

///|
test "play_card: origin カードは変化しない" {
  let field = initial_battle_field.own
  let result = play_card(field)
  assert_eq(result.origin.id, field.origin.id)
  assert_eq(result.origin.state.current_hp, field.origin.state.current_hp)
}

///|
test "play_card: FrontL が Sealed のとき FrontR に配置する" {
  let base = initial_battle_field.own
  let field = {
    origin: base.origin,
    front_l: Sealed,
    front_r: Empty,
    back_l: base.back_l,
    back_r: base.back_r,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: base.hands,
    stack: base.stack,
  }
  let hands_before = field.hands.length()
  let result = play_card(field)
  match result.front_l {
    Sealed => ()
    _ => abort("front_l は Sealed のままであるべき")
  }
  assert_eq(is_assigned(result, FrontR), true)
  assert_eq(result.hands.length(), hands_before - 1)
}
