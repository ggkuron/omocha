///|
test "worm small grid" {
  let config : WormConfig = {
    width: 25,
    height: 15,
    num_worms: 3,
    worm_length: 30,
    worm_min_width: 1,
    worm_max_width: 2,
    turn_chance: 0.3,
    room_chance: 0.05,
    room_min_size: 2,
    room_max_size: 3,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|###############....######
    #|###############.#########
    #|##############...########
    #|##############....#######
    #|############......#######
    #|############..#.#.#######
    #|############S...#..######
    #|############.#..........#
    #|############............#
    #|######################..#
    #|#######################.#
    #|#######################.#
    #|#######################G#
    #|#########################
  ))
  assert_true(result.grid.get(12, 7) == @types.Start)
}

///|
test "worm deterministic" {
  let config = WormConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "worm connectivity" {
  let config = WormConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
