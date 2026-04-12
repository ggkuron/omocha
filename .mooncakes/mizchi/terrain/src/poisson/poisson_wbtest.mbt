///|
test "poisson small grid" {
  let config : PoissonConfig = {
    width: 20,
    height: 12,
    min_distance: 6,
    room_min_size: 2,
    room_max_size: 4,
    max_attempts: 20,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|####################
      #|###############...##
      #|##########,,,,,,,.##
      #|##########,####...##
      #|##########,####...##
      #|########..,.########
      #|###.,,,,,,,.########
      #|###.,###....########
      #|###.,###############
      #|###..###############
      #|####################
      #|####################
    ),
  )
  assert_true(result.rooms.length() > 0)
}

///|
test "poisson deterministic" {
  let config = PoissonConfig::default()
  let rng1 = @types.make_rng(55U)
  let rng2 = @types.make_rng(55U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "poisson connectivity" {
  let config = PoissonConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
