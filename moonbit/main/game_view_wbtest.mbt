///|
test "Game::new initializes own side selection and initial state" {
  let game = Game::new()
  assert_eq(game.selection, @lib.SideSelect(@lib.Own))
  assert_eq(game.state.turn, @lib.Own)
  assert_eq("\{game.state.phase}", "StartPhase")
  assert_eq(game.state.battlefield.own.hands.length(), 5)
}

///|
test "is_slot_selected matches side zone and slot" {
  let selection = @lib.SlotSelect(@lib.Own, @lib.BattleZone, @lib.FrontL)
  assert_eq(
    is_slot_selected(selection, @lib.Own, @lib.BattleZone, @lib.FrontL),
    true,
  )
}

///|
test "is_slot_selected rejects same slot in a different zone" {
  let selection = @lib.SlotSelect(@lib.Own, @lib.BattleZone, @lib.FrontL)
  assert_eq(
    is_slot_selected(selection, @lib.Own, @lib.GuardZone, @lib.FrontL),
    false,
  )
}

///|
test "is_slot_selected rejects same zone and slot on a different side" {
  let selection = @lib.SlotSelect(@lib.Own, @lib.BattleZone, @lib.FrontL)
  assert_eq(
    is_slot_selected(selection, @lib.Rival, @lib.BattleZone, @lib.FrontL),
    false,
  )
}

///|
test "is_slot_selected rejects non-slot selections" {
  assert_eq(
    is_slot_selected(
      @lib.FieldSelect(@lib.Own, @lib.BattleZone),
      @lib.Own,
      @lib.BattleZone,
      @lib.FrontL,
    ),
    false,
  )
}
