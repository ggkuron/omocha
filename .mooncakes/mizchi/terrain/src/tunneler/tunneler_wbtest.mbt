///|
test "tunneler small grid" {
  let config : TunnelerConfig = {
    width: 30,
    height: 18,
    num_tunnelers: 3,
    tunnel_length: 40,
    initial_width: 1,
    max_width: 3,
    turn_chance: 0.2,
    width_change_chance: 0.1,
    room_chance: 0.06,
    room_min_size: 3,
    room_max_size: 4,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|###########,,,################
    #|###########.....,,...,,#######
    #|###########.....,,...,,#######
    #|###########.....,,...,,#######
    #|################,,...,,##,,,##
    #|################,,,,,,,##,,,##
    #|##############...,#######,,,##
    #|##############....#######,,,##
    #|##############.S..#######,...#
    #|##############....#######,...#
    #|############......#######....#
    #|############....,########...,#
    #|########...........#,####...,#
    #|########...........,,,......,#
    #|########....,,.....,,,...,,,##
    #|########....,,.....,,,...,,G##
    #|##############################
  ))
  assert_true(result.grid.get(15, 9) == @types.Start)
}

///|
test "tunneler deterministic" {
  let config = TunnelerConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "tunneler connectivity" {
  let config = TunnelerConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
