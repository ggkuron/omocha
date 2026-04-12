///|
test "prefab small grid" {
  let config : PrefabConfig = {
    width: 30,
    height: 15,
    num_prefabs: 5,
    max_attempts: 50,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(result.grid.to_ascii(), content=(
    #|##############################
    #|##############################
    #|##############################
    #|####################.#########
    #|###################...########
    #|##################..,..#######
    #|##############...##.,.########
    #|##############.S,,,,,#########
    #|##############...#,###########
    #|##################,###...#####
    #|#########.....#...,..#.,.#####
    #|###########G,,,,,,,,,,,,.#####
    #|###########.##################
    #|##############################
    #|##############################

  ))
}

///|
test "prefab deterministic" {
  let config = PrefabConfig::default()
  let rng1 = @types.make_rng(77U)
  let rng2 = @types.make_rng(77U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}

///|
test "prefab connectivity" {
  let config = PrefabConfig::default()
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  let metrics = @types.compute_metrics(result.grid, result.rooms.length())
  assert_true(metrics.connectivity)
}
