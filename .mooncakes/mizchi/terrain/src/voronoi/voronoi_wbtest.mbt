///|
test "voronoi small grid" {
  let config : VoronoiConfig = {
    width: 20,
    height: 12,
    num_seeds: 4,
    shrink: 1,
    extra_edge_chance: 0.2,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|####################
    #|#........##....##..#
    #|#.,,,,..###....##..#
    #|##,#.,.####....##..#
    #|##,##,####.....##..#
    #|#.,##,###......##..#
    #|#.,,,,,,,,S,,,,,,,.#
    #|#.,.####.....####,.#
    #|#.....##....####.,.#
    #|#.....##....###..G.#
    #|#.....##....##.....#
    #|####################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "voronoi deterministic" {
  let config = VoronoiConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "voronoi connectivity" {
  let config = VoronoiConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
