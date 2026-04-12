///|
test "graph_grammar small grid" {
  let config : GraphGrammarConfig = {
    width: 25,
    height: 15,
    num_rooms: 4,
    num_locks: 1,
    room_size: 3,
  }
  let rng = @types.make_rng(42U)
  let result = generate(config, rng)
  inspect(
    result.grid.to_ascii(),
    content=(
      #|#########################
      #|#########################
      #|########...##############
      #|########.,,,,,,,,,,,,####
      #|########.,.######,##,####
      #|#########,######.,.#,####
      #|##...##..,###,,,,,.#,####
      #|##.,,,,,,,##.,.#...#,####
      #|##...##.,,,,,,.#####,####
      #|############...####.,.###
      #|###################.,.###
      #|###################...###
      #|#########################
      #|#########################
      #|#########################
    ),
  )
  assert_true(result.rooms.length() > 0)
}

///|
test "graph_grammar deterministic" {
  let config = GraphGrammarConfig::default()
  let rng1 = @types.make_rng(42U)
  let rng2 = @types.make_rng(42U)
  let r1 = generate(config, rng1)
  let r2 = generate(config, rng2)
  assert_eq(r1.grid.to_ascii(), r2.grid.to_ascii())
}
