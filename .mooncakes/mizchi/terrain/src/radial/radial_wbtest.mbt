///|
test "radial small grid" {
  let config : RadialConfig = {
    width: 21,
    height: 21,
    num_rings: 3,
    sectors_per_ring: 6,
    ring_width: 2,
    spoke_chance: 0.7,
    gap_chance: 0.1,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##########.##########
    #|######.........######
    #|####.............####
    #|###....###.,##....###
    #|##...##....,..##...##
    #|##..##....,,...,#..##
    #|#...#,..##,##,,,#...#
    #|#..#.,,#..,..,...#..#
    #|#..#..,...,.,,#..#..#
    #|#..#..,,,###..#..#..#
    #|.........#,,,..###...
    #|#..#..#..###,.####..#
    #|#..#..#.....,.####..#
    #|#..#...#..,..#####..#
    #|#...#...##,##.###...#
    #|##..##....,##..G#..##
    #|##...##...,##.##...##
    #|###....###,###....###
    #|####.............####
    #|######.........######
    #|##########.##########

  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "radial deterministic" {
  let config = RadialConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "radial connectivity" {
  let config = RadialConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
