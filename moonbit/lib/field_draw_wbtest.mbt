fn make_battle_field_with_own_stack() -> BattleField {
  let base = initial_battle_field.own
  let n = base.hands.length()
  let new_hands : Array[HandCard] = Array::new()
  for i in 0..<(n - 1) {
    new_hands.push(base.hands[i])
  }
  let new_stack : Array[HandCard] = Array::new()
  new_stack.push(base.hands[n - 1])
  let own = {
    origin: base.origin,
    front_l: base.front_l,
    front_r: base.front_r,
    back_l: base.back_l,
    back_r: base.back_r,
    aura: base.aura,
    astra_records: base.astra_records,
    hands: new_hands,
    stack: new_stack,
  }
  { own, rival: initial_battle_field.rival }
}
fn test_draw_field_own_with_stack() -> Field {
  make_battle_field_with_own_stack().own
}
test "draw_one_card: 通常ドロー - hands が1枚増え stack が1枚減る" {
  let field = test_draw_field_own_with_stack()
  let hands_before = field.hands.length()
  let stack_before = field.stack.length()
  let result = draw_one_card(field)
  assert_eq(result.hands.length(), hands_before + 1)
  assert_eq(result.stack.length(), stack_before - 1)
}
test "draw_one_card: 空 stack では変化なし" {
  let field = initial_battle_field.rival
  let hands_before = field.hands.length()
  let stack_before = field.stack.length()
  let result = draw_one_card(field)
  assert_eq(result.hands.length(), hands_before)
  assert_eq(result.stack.length(), stack_before)
}
