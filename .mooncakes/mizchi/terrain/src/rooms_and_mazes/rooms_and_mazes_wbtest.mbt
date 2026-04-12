///|
test "rooms_and_mazes small grid" {
  let config : RoomsAndMazesConfig = {
    width: 21,
    height: 13,
    num_room_attempts: 20,
    room_min_size: 3,
    room_max_size: 5,
    winding_chance: 0.3,
    extra_connector_chance: 0.02,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#####################
    #|#######,,,,,,,#######
    #|#######,#####,#######
    #|#######,#...#,###...#
    #|#######,+...#,#,+.G.#
    #|#########...#,,,#...#
    #|#############,#+#####
    #|#######...##,,#...###
    #|#######.S.##+##...###
    #|#######...+...#...###
    #|###########...#...+##
    #|###########...#...###
    #|#####################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "rooms_and_mazes deterministic" {
  let config = RoomsAndMazesConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "rooms_and_mazes connectivity" {
  let config = RoomsAndMazesConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
