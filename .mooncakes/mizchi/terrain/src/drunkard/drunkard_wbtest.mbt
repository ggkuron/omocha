///|
test "drunkard small grid" {
  let config : DrunkardConfig = {
    width: 15,
    height: 10,
    fill_ratio: 0.3,
    max_steps: 1000,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|###############
      #|###############
      #|###....########
      #|#.......#######
      #|#........######
      #|#........######
      #|#.....#..######
      #|#.....##.######
      #|#.....#########
      #|###############
    ),
  )
}

///|
test "drunkard deterministic" {
  let config = DrunkardConfig::default()
  let rng1 = @types.make_rng(123U)
  let rng2 = @types.make_rng(123U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "drunkard connectivity" {
  let config : DrunkardConfig = {
    width: 20,
    height: 15,
    fill_ratio: 0.35,
    max_steps: 3000,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, 0)
  assert_true(metrics.connectivity)
}
