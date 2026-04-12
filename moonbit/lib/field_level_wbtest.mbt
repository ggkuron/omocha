test "next_in_level: SideSelect(Own) → SideSelect(Rival)" {
  let bf = initial_battle_field
  assert_eq(next_in_level(bf, SideSelect(Own)), SideSelect(Rival))
}
test "next_in_level: SideSelect(Rival) → SideSelect(Own) - wrap around" {
  let bf = initial_battle_field
  assert_eq(next_in_level(bf, SideSelect(Rival)), SideSelect(Own))
}
test "next_in_level: FieldSelect BattleZone → AuraZone" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, FieldSelect(Own, BattleZone)),
    FieldSelect(Own, AuraZone),
  )
}
test "next_in_level: FieldSelect GuardZone → BattleZone - wrap around" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, FieldSelect(Own, GuardZone)),
    FieldSelect(Own, BattleZone),
  )
}
test "next_in_level: SlotSelect rival OriginSlot → FrontL" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, SlotSelect(Rival, BattleZone, OriginSlot)),
    SlotSelect(Rival, BattleZone, FrontL),
  )
}
test "next_in_level: SlotSelect rival FrontL → FrontR" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, SlotSelect(Rival, BattleZone, FrontL)),
    SlotSelect(Rival, BattleZone, FrontR),
  )
}
test "next_in_level: SlotSelect rival FrontR → BackL" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, SlotSelect(Rival, BattleZone, FrontR)),
    SlotSelect(Rival, BattleZone, BackL),
  )
}
test "next_in_level: SlotSelect rival BackL → OriginSlot - BackR(Empty) をスキップ" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, SlotSelect(Rival, BattleZone, BackL)),
    SlotSelect(Rival, BattleZone, OriginSlot),
  )
}
test "next_in_level: own 全スロット Empty - OriginSlot のまま" {
  let bf = initial_battle_field
  assert_eq(
    next_in_level(bf, SlotSelect(Own, BattleZone, OriginSlot)),
    SlotSelect(Own, BattleZone, OriginSlot),
  )
}
test "prev_in_level: SideSelect(Rival) → SideSelect(Own)" {
  let bf = initial_battle_field
  assert_eq(prev_in_level(bf, SideSelect(Rival)), SideSelect(Own))
}
test "prev_in_level: SideSelect(Own) → SideSelect(Rival) - wrap around" {
  let bf = initial_battle_field
  assert_eq(prev_in_level(bf, SideSelect(Own)), SideSelect(Rival))
}
test "prev_in_level: FieldSelect AuraZone → BattleZone" {
  let bf = initial_battle_field
  assert_eq(
    prev_in_level(bf, FieldSelect(Own, AuraZone)),
    FieldSelect(Own, BattleZone),
  )
}
test "prev_in_level: FieldSelect BattleZone → GuardZone - wrap around" {
  let bf = initial_battle_field
  assert_eq(
    prev_in_level(bf, FieldSelect(Own, BattleZone)),
    FieldSelect(Own, GuardZone),
  )
}
test "prev_in_level: SlotSelect rival OriginSlot → BackL - BackR(Empty) をスキップ" {
  let bf = initial_battle_field
  assert_eq(
    prev_in_level(bf, SlotSelect(Rival, BattleZone, OriginSlot)),
    SlotSelect(Rival, BattleZone, BackL),
  )
}
test "prev_in_level: SlotSelect rival FrontL → OriginSlot" {
  let bf = initial_battle_field
  assert_eq(
    prev_in_level(bf, SlotSelect(Rival, BattleZone, FrontL)),
    SlotSelect(Rival, BattleZone, OriginSlot),
  )
}
test "prev_in_level: SlotSelect rival BackL → FrontR" {
  let bf = initial_battle_field
  assert_eq(
    prev_in_level(bf, SlotSelect(Rival, BattleZone, BackL)),
    SlotSelect(Rival, BattleZone, FrontR),
  )
}
test "nextInLevel/prevInLevel 対称性: SideSelect(Own)" {
  let bf = initial_battle_field
  let original = SideSelect(Own)
  assert_eq(prev_in_level(bf, next_in_level(bf, original)), original)
}
test "nextInLevel/prevInLevel 対称性: FieldSelect(Own, BattleZone)" {
  let bf = initial_battle_field
  let original = FieldSelect(Own, BattleZone)
  assert_eq(prev_in_level(bf, next_in_level(bf, original)), original)
}
test "nextInLevel/prevInLevel 対称性: SlotSelect rival OriginSlot" {
  let bf = initial_battle_field
  let original = SlotSelect(Rival, BattleZone, OriginSlot)
  assert_eq(prev_in_level(bf, next_in_level(bf, original)), original)
}
test "nextInLevel/prevInLevel 対称性: own stack あり FieldSelect" {
  let bf = make_battle_field_with_own_stack()
  let original = FieldSelect(Own, AuraZone)
  assert_eq(prev_in_level(bf, next_in_level(bf, original)), original)
}
