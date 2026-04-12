///|
test "wfc small grid" {
  let config : WfcConfig = { width: 12, height: 8, max_retries: 10 }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|############
      #|#..#,.#.,,,#
      #|#,,,,#####.#
      #|##,,.#..,,,#
      #|#,.,,#,.##,#
      #|#.,..#.,##,#
      #|#.,,,,...###
      #|############
    ),
  )
  inspect(result.success, content="true")
}

///|
test "wfc deterministic" {
  let config = WfcConfig::default()
  let rng1 = @types.make_rng(42U)
  let rng2 = @types.make_rng(42U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}
