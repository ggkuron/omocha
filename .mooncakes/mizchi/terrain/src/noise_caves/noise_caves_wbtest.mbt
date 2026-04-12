///|
test "noise_caves small grid" {
  let config : NoiseCavesConfig = {
    width: 25,
    height: 15,
    threshold: 0.45,
    octaves: 3,
    scale: 0.1,
    persistence: 0.5,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|#########################
    #|#########################
    #|##G.#####################
    #|#....####################
    #|#...#####################
    #|#................########
    #|#.##############..#######
    #|##########.###.....######
    #|##########..S.......#.###
    #|##############.........##
    #|##############.........##
    #|##############.......####
    #|################..#######
    #|#########################
    #|#########################
  ))
  assert_true(result.open_cells > 0)
}

///|
test "noise_caves deterministic" {
  let config = NoiseCavesConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "noise_caves connectivity" {
  let config = NoiseCavesConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, 0)
  assert_true(metrics.connectivity)
}
