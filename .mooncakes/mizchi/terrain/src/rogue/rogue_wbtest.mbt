///|
test "rogue small grid" {
  let config : RogueConfig = {
    width: 30,
    height: 18,
    grid_cols: 3,
    grid_rows: 3,
    room_min_ratio: 0.4,
    room_max_ratio: 0.8,
    skip_chance: 0.1,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|##############################
    #|###############...#####.....##
    #|###############.S+,,,,,+,,..##
    #|###############.+.#####..,..##
    #|################,######..+..##
    #|################,########,####
    #|###############.+.#######,####
    #|###.....#######.,.######.+.###
    #|###..,,+,,,,,,,+,+,,,,,,+,.###
    #|###...+.######,+..#####,+..###
    #|######,#######,########,######
    #|####..+.####..+.######.+.#####
    #|####..,.####..,+,,,,,,+,.#####
    #|####..,+,,,,+,,.######.G.#####
    #|####....##############...#####
    #|##############################
    #|##############################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "rogue deterministic" {
  let config = RogueConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "rogue connectivity" {
  let config = RogueConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
