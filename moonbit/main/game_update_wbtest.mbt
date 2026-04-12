///|
fn input_with_key(key : Int) -> @core.InputSnapshot {
  @core.new_input_snapshot(0.0, 0.0, 0.0, 0.0, [key])
}

///|
fn assigned_id_for_game_update(slot : @lib.SatelliteSlot) -> Int {
  match slot {
    @lib.Assigned(inst) => inst.id
    _ => abort("expected assigned satellite slot")
  }
}

///|
test "update_game handles right key through selection entrypoint" {
  let game = Game::new()
  game.update_game(input_with_key(@inpututil.key_right_web))
  assert_eq(game.selection, @lib.FieldSelect(@lib.Own, @lib.BattleZone))
}

///|
test "update_game handles left key through selection entrypoint" {
  let game = Game::new()
  game.selection = @lib.SlotSelect(@lib.Own, @lib.BattleZone, @lib.OriginSlot)
  game.update_game(input_with_key(@inpututil.key_left_web))
  assert_eq(game.selection, @lib.FieldSelect(@lib.Own, @lib.BattleZone))
}

///|
test "update_game handles up key through selection entrypoint" {
  let game = Game::new()
  game.update_game(input_with_key(@inpututil.key_up_web))
  assert_eq(game.selection, @lib.SideSelect(@lib.Rival))
}

///|
test "update_game handles down key through selection entrypoint" {
  let game = Game::new()
  game.update_game(input_with_key(@inpututil.key_down_web))
  assert_eq(game.selection, @lib.SideSelect(@lib.Rival))
}

///|
test "update_game handles space confirm through card play entrypoint" {
  let game = Game::new()
  let hand_count = game.state.battlefield.own.hands.length()
  game.update_game(input_with_key(@inpututil.key_space))
  assert_eq(
    assigned_id_for_game_update(game.state.battlefield.own.front_l),
    101,
  )
  assert_eq(game.state.battlefield.own.hands.length(), hand_count - 1)
}

///|
test "update_game handles enter confirm through card play entrypoint" {
  let game = Game::new()
  game.update_game(input_with_key(@inpututil.key_enter_web))
  assert_eq(
    assigned_id_for_game_update(game.state.battlefield.own.front_l),
    101,
  )
}

///|
test "update_game handles E key through turn entrypoint" {
  let game = Game::new()
  game.update_game(input_with_key(@inpututil.key_e))
  assert_eq(game.state.turn, @lib.Rival)
  assert_eq(game.state.log.length(), 5)
}
