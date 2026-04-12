///|
pub(all) struct CellularConfig {
  width : Int
  height : Int
  initial_fill : Double
  iterations : Int
  birth_threshold : Int
  survival_threshold : Int
}

///|
pub fn CellularConfig::default() -> CellularConfig {
  {
    width: 40,
    height: 20,
    initial_fill: 0.45,
    iterations: 5,
    birth_threshold: 5,
    survival_threshold: 4,
  }
}

///|
pub(all) struct CellularResult {
  grid : @types.Grid2D
  iterations_run : Int
}

///|
pub fn generate(config : CellularConfig, rng : @types.RNG) -> CellularResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Random initial fill
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if rng.rand_bool(config.initial_fill) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Cellular automata iterations
  for iter in 0..<config.iterations {
    ignore(iter)
    let old = grid.clone()
    for y in 1..<(config.height - 1) {
      for x in 1..<(config.width - 1) {
        let walls = old.count_neighbors(x, y, @types.Wall)
        if old.get(x, y) == @types.Wall {
          // Birth rule
          if walls < config.birth_threshold {
            grid.set(x, y, @types.Floor)
          }
        } else {
          // Death rule
          if walls >= config.survival_threshold {
            grid.set(x, y, @types.Wall)
          }
        }
      }
    }
  }
  { grid, iterations_run: config.iterations }
}
