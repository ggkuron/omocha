///|
test "arena_chain small grid" {
  let config : ArenaChainConfig = {
    width: 30,
    height: 20,
    num_arenas: 3,
    arena_min_size: 4,
    arena_max_size: 6,
    corridor_width: 1,
    side_room_chance: 0.3,
    side_room_min_size: 2,
    side_room_max_size: 3,
    max_placement_attempts: 50,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|##############################
    #|#####....#####################
    #|#####....###....##############
    #|#####....###....##############
    #|#####..S+,,,+,,,##############
    #|#####....###...+##############
    #|#####....######,##############
    #|###############,##############
    #|###############,##############
    #|###############,##############
    #|###############,##############
    #|###############,##############
    #|#############..+..############
    #|#############..,..############
    #|#############..,..############
    #|#############..G..############
    #|#############.....############
    #|#############.....############
    #|##############################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "arena_chain deterministic" {
  let config = ArenaChainConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "arena_chain connectivity" {
  let config = ArenaChainConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
