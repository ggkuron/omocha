///|
test "dla small grid" {
  let config : DlaConfig = {
    width: 20,
    height: 12,
    target_fill: 0.3,
    max_walk_steps: 300,
    num_seeds: 2,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|####################
    #|###G#.##############
    #|###.....############
    #|###...........#.####
    #|###..######......###
    #|###..##.........####
    #|###...###.S..#######
    #|###.#######...######
    #|##########....######
    #|##########....######
    #|#########...#...####
    #|####################
  ))
  assert_true(result.particles > 0)
}

///|
test "dla deterministic" {
  let config = DlaConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "dla connectivity" {
  let config = DlaConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, 0)
  assert_true(metrics.connectivity)
}
