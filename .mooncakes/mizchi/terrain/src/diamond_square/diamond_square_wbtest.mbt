///|
test "diamond_square small grid" {
  let config : DiamondSquareConfig = {
    width: 21,
    height: 13,
    roughness: 0.6,
    threshold: 0.45,
    initial_range: 1.0,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#####################
    #|#,S...###############
    #|#,.......############
    #|#,..#....############
    #|#,,,,,,,,,,,,,,,,,,.#
    #|#,.......##########.#
    #|#,......##########..#
    #|#,#.G################
    #|#,,,,,,,,,,,,,,,,,###
    #|#####################
    #|#####################
    #|#####################
    #|#####################

  ))
}

///|
test "diamond_square deterministic" {
  let config = DiamondSquareConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "diamond_square connectivity" {
  let config = DiamondSquareConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
