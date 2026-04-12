///|
test "feature_growing small grid" {
  let config : FeatureGrowingConfig = {
    width: 30,
    height: 18,
    max_features: 15,
    max_attempts: 300,
    room_min_size: 3,
    room_max_size: 5,
    corridor_min_len: 2,
    corridor_max_len: 5,
    room_weight: 3,
    corridor_weight: 4,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|########################...###
    #|#####################,,+...###
    #|########################...###
    #|###################....+...###
    #|###################....#...###
    #|###################....#######
    #|######...#...#...##+##+#######
    #|######.G.#...+...+,,##,#######
    #|######...+...#.S.#####,+...###
    #|##########...#...+,,,#,#...###
    #|#####,,,,+...#...#####,#...###
    #|##############+#######+#...###
    #|########,,+....#######,#######
    #|###########....#######,#######
    #|########,,+....###############
    #|##############################
    #|##############################
  ))
  assert_true(result.rooms.length() > 1)
}

///|
test "feature_growing deterministic" {
  let config = FeatureGrowingConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "feature_growing connectivity" {
  let config = FeatureGrowingConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
