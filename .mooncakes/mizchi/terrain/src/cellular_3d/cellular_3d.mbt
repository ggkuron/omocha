///|
pub(all) struct Cellular3dConfig {
  width : Int
  height : Int
  depth : Int
  initial_fill : Double
  iterations : Int
  birth : Int
  survival : Int
}

///|
pub fn Cellular3dConfig::default() -> Cellular3dConfig {
  {
    width: 40,
    height: 20,
    depth: 3,
    initial_fill: 0.45,
    iterations: 4,
    birth: 14,
    survival: 13,
  }
}

///|
pub(all) struct Cellular3dResult {
  grid : @types.Grid3D
  open_cells : Int
}

///|
fn count_wall_neighbors_3d(
  grid : @types.Grid3D,
  x : Int,
  y : Int,
  z : Int
) -> Int {
  let mut count = 0
  // Check all 26 neighbors in 3D (3x3x3 cube minus center)
  for dz in -1..=1 {
    for dy in -1..=1 {
      for dx in -1..=1 {
        if dx == 0 && dy == 0 && dz == 0 {
          continue
        }
        let nx = x + dx
        let ny = y + dy
        let nz = z + dz
        if nx >= 0 &&
          nx < grid.width &&
          ny >= 0 &&
          ny < grid.height &&
          nz >= 0 &&
          nz < grid.depth {
          if grid.get(nx, ny, nz) == @types.Wall {
            count += 1
          }
        } else {
          // Out of bounds counts as wall
          count += 1
        }
      }
    }
  }
  count
}

///|
pub fn generate(
  config : Cellular3dConfig,
  rng : @types.RNG
) -> Cellular3dResult {
  let grid = @types.Grid3D::make(
    config.width, config.height, config.depth, @types.Wall,
  )
  // Random initial fill on all layers
  for z in 0..<config.depth {
    for y in 1..<(config.height - 1) {
      for x in 1..<(config.width - 1) {
        if rng.rand_bool(config.initial_fill) {
          grid.set(x, y, z, @types.Floor)
        }
      }
    }
  }
  // Run cellular automata iterations
  for _iter in 0..<config.iterations {
    // Snapshot old state: clone each layer
    let old_layers : Array[@types.Grid2D] = []
    for z in 0..<config.depth {
      old_layers.push(grid.get_layer(z).clone())
    }
    let old = @types.Grid3D::make(config.width, config.height, config.depth, @types.Wall)
    for z in 0..<config.depth {
      for y in 0..<config.height {
        for x in 0..<config.width {
          old.set(x, y, z, old_layers[z].get(x, y))
        }
      }
    }
    // Apply rules
    for z in 0..<config.depth {
      for y in 1..<(config.height - 1) {
        for x in 1..<(config.width - 1) {
          let walls = count_wall_neighbors_3d(old, x, y, z)
          if old.get(x, y, z) == @types.Wall {
            // Birth: wall becomes floor if wall neighbors < birth threshold
            if walls < config.birth {
              grid.set(x, y, z, @types.Floor)
            }
          } else {
            // Survival: floor becomes wall if wall neighbors >= survival threshold
            if walls >= config.survival {
              grid.set(x, y, z, @types.Wall)
            }
          }
        }
      }
    }
  }
  // Count open cells
  let mut open_cells = 0
  for z in 0..<config.depth {
    for y in 0..<config.height {
      for x in 0..<config.width {
        if grid.get(x, y, z) == @types.Floor {
          open_cells += 1
        }
      }
    }
  }
  { grid, open_cells }
}
