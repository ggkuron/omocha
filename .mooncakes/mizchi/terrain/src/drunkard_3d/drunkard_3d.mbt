///|
pub(all) struct Drunkard3dConfig {
  width : Int
  height : Int
  depth : Int
  fill_ratio : Double
  climb_chance : Double
}

///|
pub fn Drunkard3dConfig::default() -> Drunkard3dConfig {
  { width: 40, height: 20, depth: 3, fill_ratio: 0.3, climb_chance: 0.1 }
}

///|
pub(all) struct Drunkard3dResult {
  grid : @types.Grid3D
  steps_taken : Int
}

///|
pub fn generate(
  config : Drunkard3dConfig,
  rng : @types.RNG
) -> Drunkard3dResult {
  let grid = @types.Grid3D::make(
    config.width, config.height, config.depth, @types.Wall,
  )
  let total_cells = config.width * config.height * config.depth
  let target_floors = (total_cells.to_double() * config.fill_ratio).to_int()
  let mut x = config.width / 2
  let mut y = config.height / 2
  let mut z = config.depth / 2
  let mut floors = 0
  let mut steps = 0
  let max_steps = total_cells * 10
  // Carve start position
  grid.set(x, y, z, @types.Floor)
  floors += 1
  while floors < target_floors && steps < max_steps {
    steps += 1
    // Decide whether to climb up/down or walk horizontally
    if rng.rand_bool(config.climb_chance) {
      // Vertical movement
      let dz = if rng.rand_bool(0.5) { 1 } else { -1 }
      let nz = z + dz
      if nz >= 0 && nz < config.depth {
        z = nz
        if grid.get(x, y, z) == @types.Wall {
          grid.set(x, y, z, @types.Floor)
          floors += 1
        }
      }
    } else {
      // Horizontal movement (4 directions)
      let dir = rng.rand_direction()
      let nx = x + dir.x
      let ny = y + dir.y
      if nx > 0 && nx < config.width - 1 && ny > 0 && ny < config.height - 1 {
        x = nx
        y = ny
        if grid.get(x, y, z) == @types.Wall {
          grid.set(x, y, z, @types.Floor)
          floors += 1
        }
      }
    }
  }
  { grid, steps_taken: steps }
}
