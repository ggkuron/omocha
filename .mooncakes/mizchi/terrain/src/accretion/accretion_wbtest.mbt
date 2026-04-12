///|
test "accretion small grid" {
  let config : AccretionConfig = {
    width: 25,
    height: 15,
    max_rooms: 6,
    room_min_size: 3,
    room_max_size: 5,
    max_attempts_per_room: 100,
    first_room_size: 4,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|#########################
    #|#########################
    #|#####....################
    #|#...#....################
    #|#...#....+....#...#....##
    #|#.G.+....#....#...+....##
    #|#...######..S.+...#....##
    #|##########....#...#######
    #|##########+####...#######
    #|########.....############
    #|########.....############
    #|########.....############
    #|#########################
    #|#########################
  ))
  assert_true(result.rooms.length() > 1)
}

///|
test "accretion deterministic" {
  let config = AccretionConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "accretion connectivity" {
  let config = AccretionConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
