test "next_selection: SideSelect(Own) → FieldSelect(Own, BattleZone)" {
  assert_eq(next_selection(SideSelect(Own)), FieldSelect(Own, BattleZone))
}
test "next_selection: SideSelect(Rival) → FieldSelect(Rival, BattleZone)" {
  assert_eq(next_selection(SideSelect(Rival)), FieldSelect(Rival, BattleZone))
}
test "next_selection: FieldSelect(Own, BattleZone) → SlotSelect(Own, BattleZone, OriginSlot)" {
  assert_eq(
    next_selection(FieldSelect(Own, BattleZone)),
    SlotSelect(Own, BattleZone, OriginSlot),
  )
}
test "next_selection: FieldSelect(Rival, AuraZone) は描画対象のまま維持" {
  assert_eq(
    next_selection(FieldSelect(Rival, AuraZone)),
    FieldSelect(Rival, AuraZone),
  )
}
test "next_selection: FieldSelect(Own, GuardZone) は描画対象のまま維持" {
  assert_eq(
    next_selection(FieldSelect(Own, GuardZone)),
    FieldSelect(Own, GuardZone),
  )
}
test "next_selection: SlotSelect は最深レベル - 変化なし" {
  let sel = SlotSelect(Own, BattleZone, OriginSlot)
  assert_eq(next_selection(sel), sel)
}
test "prev_selection: SlotSelect → FieldSelect (一段上)" {
  assert_eq(
    prev_selection(SlotSelect(Own, BattleZone, OriginSlot)),
    FieldSelect(Own, BattleZone),
  )
}
test "prev_selection: SlotSelect(Rival, AuraZone, FrontL) → FieldSelect(Rival, AuraZone)" {
  assert_eq(
    prev_selection(SlotSelect(Rival, AuraZone, FrontL)),
    FieldSelect(Rival, AuraZone),
  )
}
test "prev_selection: FieldSelect → SideSelect (一段上)" {
  assert_eq(prev_selection(FieldSelect(Own, BattleZone)), SideSelect(Own))
}
test "prev_selection: FieldSelect(Rival, GuardZone) → SideSelect(Rival)" {
  assert_eq(prev_selection(FieldSelect(Rival, GuardZone)), SideSelect(Rival))
}
test "prev_selection: SideSelect(Own) は最上レベル - 変化なし" {
  assert_eq(prev_selection(SideSelect(Own)), SideSelect(Own))
}
test "prev_selection: SideSelect(Rival) は最上レベル - 変化なし" {
  assert_eq(prev_selection(SideSelect(Rival)), SideSelect(Rival))
}
test "next/prev 往復対称性: SideSelect(Own)" {
  let original = SideSelect(Own)
  assert_eq(prev_selection(next_selection(original)), original)
}
test "next/prev 往復対称性: FieldSelect(Own, BattleZone)" {
  let original = FieldSelect(Own, BattleZone)
  assert_eq(prev_selection(next_selection(original)), original)
}
test "is_assigned: OriginSlot は常に true" {
  assert_eq(is_assigned(initial_battle_field.own, OriginSlot), true)
  assert_eq(is_assigned(initial_battle_field.rival, OriginSlot), true)
}
test "is_assigned: own の衛星スロットは全て false" {
  let field = initial_battle_field.own
  assert_eq(is_assigned(field, FrontL), false)
  assert_eq(is_assigned(field, FrontR), false)
  assert_eq(is_assigned(field, BackL), false)
  assert_eq(is_assigned(field, BackR), false)
}
test "is_assigned: rival frontL/frontR/backL は true, backR は false" {
  let field = initial_battle_field.rival
  assert_eq(is_assigned(field, FrontL), true)
  assert_eq(is_assigned(field, FrontR), true)
  assert_eq(is_assigned(field, BackL), true)
  assert_eq(is_assigned(field, BackR), false)
}
