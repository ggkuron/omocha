///|
test "diablo small grid" {
  let config : DiabloConfig = {
    width: 30,
    height: 20,
    spine_length: 4,
    branch_chance: 0.5,
    loop_chance: 0.25,
    room_min_size: 3,
    room_max_size: 5,
    room_padding: 1,
    max_placement_attempts: 50,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|######...#####...####....#####
    #|######.,+,,,,,+,+,,,,+,,.#####
    #|######...#####.+.####..,,#####
    #|###############,#####..++..###
    #|###############,######.,,..###
    #|###############,######.,G..###
    #|###############,######.,...###
    #|#############..+.#####.+...###
    #|#############..,.######,######
    #|#############..S.######,######
    #|#############....######,######
    #|#######################,######
    #|######################.+.#####
    #|######################.,.#####
    #|######################...#####
    #|##############################
    #|##############################
    #|##############################
    #|##############################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "diablo deterministic" {
  let config : DiabloConfig = {
    width: 30,
    height: 20,
    spine_length: 4,
    branch_chance: 0.5,
    loop_chance: 0.25,
    room_min_size: 3,
    room_max_size: 5,
    room_padding: 1,
    max_placement_attempts: 50,
  }
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "diablo connectivity" {
  let config = DiabloConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
