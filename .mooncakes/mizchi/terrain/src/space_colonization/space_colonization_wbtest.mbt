///|
test "space_colonization small grid" {
  let config : SpaceColonizationConfig = {
    width: 25,
    height: 15,
    num_attractors: 30,
    kill_distance: 2,
    influence_distance: 10,
    step_size: 1,
    corridor_width: 1,
    room_chance: 0.0,
    room_min_size: 3,
    room_max_size: 3,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|#########################
    #|#########################
    #|#########################
    #|###,###############,,,G##
    #|###,,,,############,,,,##
    #|###,,,,#####,,,,##,,,,,##
    #|###,,,,,,,,,S,,,,,,,,,,##
    #|###,,,,,,,,,,,,,,,,,,,,##
    #|###,,######,,,,,,,,,,,,##
    #|###,,######,,,,##########
    #|########,,,,,,,##########
    #|#########################
    #|#########################
    #|#########################

  ))
}

///|
test "space_colonization deterministic" {
  let config = SpaceColonizationConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "space_colonization connectivity" {
  let config = SpaceColonizationConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
