///|
fn assigned_id_for_state_test(slot : SatelliteSlot) -> Int {
  match slot {
    Assigned(inst) => inst.id
    _ => abort("expected assigned satellite slot")
  }
}

///|
test "apply_play_own_card places own hand head into FrontL" {
  let state = initial_game_state()
  let result = apply_play_own_card(state)
  assert_eq(assigned_id_for_state_test(result.battlefield.own.front_l), 101)
  assert_eq(
    result.battlefield.own.hands.length(),
    state.battlefield.own.hands.length() - 1,
  )
}

///|
test "apply_play_own_card leaves rival field unchanged" {
  let state = initial_game_state()
  let result = apply_play_own_card(state)
  assert_eq(
    assigned_id_for_state_test(result.battlefield.rival.front_l),
    assigned_id_for_state_test(state.battlefield.rival.front_l),
  )
  assert_eq(
    assigned_id_for_state_test(result.battlefield.rival.front_r),
    assigned_id_for_state_test(state.battlefield.rival.front_r),
  )
  assert_eq(
    assigned_id_for_state_test(result.battlefield.rival.back_l),
    assigned_id_for_state_test(state.battlefield.rival.back_l),
  )
  assert_eq(
    result.battlefield.rival.hands.length(),
    state.battlefield.rival.hands.length(),
  )
}

///|
test "apply_play_own_card preserves turn phase and log" {
  let state = initial_game_state()
  let result = apply_play_own_card(state)
  assert_eq(result.turn, state.turn)
  assert_eq(result.phase, state.phase)
  assert_eq(result.log.length(), state.log.length())
}
