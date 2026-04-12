///|
test "superimpose small grid" {
  let config : SuperimposeConfig = {
    width: 25,
    height: 15,
    num_stamps: 20,
    rect_weight: 3,
    circle_weight: 2,
    corridor_h_weight: 2,
    corridor_v_weight: 2,
    stamp_min_size: 2,
    stamp_max_size: 5,
    corridor_min_len: 3,
    corridor_max_len: 8,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|#G...................####
    #|#....###########......###
    #|#...############......###
    #|##.##################.###
    #|##.##################.###
    #|#####################.###
    #|#.####......S...#####.###
    #|#.......##............###
    #|#.......##.......#.....##
    #|#.......###.....##.....##
    #|#.......###.....##.....##
    #|##......##########.....##
    #|#########################
    #|#########################
  ))
  assert_true(result.rooms.length() > 0)
}

///|
test "superimpose deterministic" {
  let config = SuperimposeConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "superimpose connectivity" {
  let config = SuperimposeConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
