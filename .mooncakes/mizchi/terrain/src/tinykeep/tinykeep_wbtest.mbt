///|
test "tinykeep small grid" {
  let config : TinyKeepConfig = {
    width: 30,
    height: 20,
    num_rooms: 15,
    room_min_size: 3,
    room_max_size: 7,
    main_room_threshold: 5,
    extra_edge_chance: 0.15,
    separation_iterations: 30,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|############..........########
    #|############............######
    #|############............######
    #|############...,,,,,,,,,,,,###
    #|############...,.....,,,,,,###
    #|############...,........##,###
    #|###############,#.......##,###
    #|#######.......#,###.....##,###
    #|#######.......#,##########,###
    #|#######.......#...#####...,..#
    #|#######...,,,,,.G.#####...,..#
    #|#######...,...#...#####...,..#
    #|#######...,...#########...,..#
    #|#######...,...#.......#...,..#
    #|########..,S,,,,,,,...#...,..#
    #|########......#...,,,,,,,,,..#
    #|########......#.......########
    #|########......#.......########
    #|##############################
  ))
  assert_true(result.rooms.length() > 1)
}

///|
test "tinykeep deterministic" {
  let config = TinyKeepConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "tinykeep connectivity" {
  let config = TinyKeepConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
