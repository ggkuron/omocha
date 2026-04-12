///|
test "cellular small grid" {
  let config : CellularConfig = {
    width: 15,
    height: 10,
    initial_fill: 0.45,
    iterations: 4,
    birth_threshold: 5,
    survival_threshold: 4,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|###############
      #|###########.###
      #|#########...###
      #|#########....##
      #|#######.....###
      #|###########.###
      #|########.######
      #|###############
      #|###############
      #|###############
    ),
  )
}

///|
test "cellular deterministic" {
  let config = CellularConfig::default()
  let rng1 = @types.make_rng(99U)
  let rng2 = @types.make_rng(99U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}
