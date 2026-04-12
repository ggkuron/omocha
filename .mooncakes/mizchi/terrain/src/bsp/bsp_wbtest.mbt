///|
test "bsp small grid" {
  let config : BspConfig = {
    width: 20,
    height: 12,
    min_room_size: 3,
    max_depth: 3,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|####################
      #|#......#.....#...###
      #|#.,,,,,,,,,,,,,,.###
      #|#..,...#.....#...###
      #|#..,.#####...#######
      #|#..,.#...#.,,,######
      #|#..,,,,,,,,,.,######
      #|#....#.,.#...,...###
      #|######...#...,...###
      #|######...#.......###
      #|####################
      #|####################
    ),
  )
  assert_true(result.rooms.length() > 0)
}

///|
test "bsp deterministic" {
  let config = BspConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "bsp connectivity" {
  let config = BspConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
