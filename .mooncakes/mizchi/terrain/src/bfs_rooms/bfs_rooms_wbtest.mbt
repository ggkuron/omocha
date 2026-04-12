///|
test "bfs_rooms small grid" {
  let config : BfsRoomsConfig = {
    width: 25,
    height: 15,
    num_seeds: 5,
    min_room_size: 5,
    parallel_steps: 8,
    serial_steps: 10,
    door_chance: 0.8,
    extra_door_chance: 0.15,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|#++++++++++++++++++.....#
    #|#.....##.......+##......#
    #|#.....####.....++#......#
    #|#......#++++++++#...G...#
    #|#.....##..##...+#.......#
    #|#....##....#+##+##......#
    #|#...##......#+++.##.....#
    #|#..##....S...##...##....#
    #|#..##.......##.....######
    #|#..##......##.......#####
    #|#..##.....##............#
    #|#..##.....##............#
    #|#..##.....##............#
    #|#########################

  ))
  assert_true(result.rooms.length() > 1)
}

///|
test "bfs_rooms deterministic" {
  let config = BfsRoomsConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "bfs_rooms connectivity" {
  let config = BfsRoomsConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
