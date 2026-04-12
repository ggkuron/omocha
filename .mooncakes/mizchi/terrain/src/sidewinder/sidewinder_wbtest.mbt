///|
test "sidewinder small grid" {
  let config : SidewinderConfig = {
    width: 21,
    height: 13,
    carve_chance: 0.5,
    room_chance: 0.0,
    room_min_size: 3,
    room_max_size: 3,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#####################
    #|#S,,,,,,,,,,,,,,,,,,#
    #|#,#,#######,#,#,###,#
    #|#,#,#,,,,,,,#,#,,,#,#
    #|#,#####,#,#,#,#,#,###
    #|#,,,#,,,#,#,#,#,#,,,#
    #|#,#######,#,#########
    #|#,,,,,,,#,#,,,,,,,,,#
    #|#,#,###,#,###,#####,#
    #|#,#,#,,,#,#,,,,,,,#,#
    #|#,#####,#,#,###,#####
    #|#,,,#,,,#,#,#,,,,,,G#
    #|#####################

  ))
}

///|
test "sidewinder deterministic" {
  let config = SidewinderConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "sidewinder connectivity" {
  let config = SidewinderConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
