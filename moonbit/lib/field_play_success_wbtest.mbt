///|
test "play_card: 全スロット Empty のとき FrontL に先頭 Satellite を配置する" {
  let field = initial_battle_field.own
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(is_assigned(result, FrontL), true)
  assert_eq(slot_instance_id(result.front_l), 101)
  assert_eq(result.hands.length(), hands_before - 1)
  assert_eq(is_assigned(result, FrontR), false)
  assert_eq(is_assigned(result, BackL), false)
  assert_eq(is_assigned(result, BackR), false)
}

///|
test "play_card: FrontL のみ Empty のとき FrontL に配置する" {
  let field = make_only_frontl_empty_own_field()
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(is_assigned(result, FrontL), true)
  assert_eq(result.hands.length(), hands_before - 1)
  assert_eq(slot_instance_id(result.front_r), slot_instance_id(field.front_r))
  assert_eq(slot_instance_id(result.back_l), slot_instance_id(field.back_l))
  assert_eq(slot_instance_id(result.back_r), slot_instance_id(field.back_r))
}

///|
test "play_card: FrontL/FrontR Assigned のとき BackL に配置する" {
  let field = make_frontl_frontr_assigned_own_field()
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(is_assigned(result, BackL), true)
  assert_eq(result.hands.length(), hands_before - 1)
  assert_eq(slot_instance_id(result.front_l), slot_instance_id(field.front_l))
  assert_eq(slot_instance_id(result.front_r), slot_instance_id(field.front_r))
  assert_eq(is_assigned(result, BackR), false)
}

///|
test "play_card: FrontL/FrontR/BackL Assigned のとき BackR に配置する" {
  let field = make_only_backr_empty_own_field()
  let hands_before = field.hands.length()
  let result = play_card(field)
  assert_eq(is_assigned(result, BackR), true)
  assert_eq(result.hands.length(), hands_before - 1)
}
