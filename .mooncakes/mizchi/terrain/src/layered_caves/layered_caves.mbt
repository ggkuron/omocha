///|
pub(all) struct LayeredCavesConfig {
  width : Int
  height : Int
  depth : Int
  connectivity : Double
}

///|
pub fn LayeredCavesConfig::default() -> LayeredCavesConfig {
  { width: 40, height: 20, depth: 3, connectivity: 0.3 }
}

///|
pub(all) struct LayeredCavesResult {
  grid : @types.Grid3D
  open_cells : Int
}

///|
fn generate_cave_layer(
  layer : @types.Grid2D,
  width : Int,
  height : Int,
  rng : @types.RNG
) -> Unit {
  let initial_fill = 0.45
  let iterations = 5
  let birth_threshold = 5
  let survival_threshold = 4
  // Random initial fill
  for y in 1..<(height - 1) {
    for x in 1..<(width - 1) {
      if rng.rand_bool(initial_fill) {
        layer.set(x, y, @types.Floor)
      }
    }
  }
  // Cellular automata smoothing
  for _iter in 0..<iterations {
    let old = layer.clone()
    for y in 1..<(height - 1) {
      for x in 1..<(width - 1) {
        let walls = old.count_neighbors(x, y, @types.Wall)
        if old.get(x, y) == @types.Wall {
          if walls < birth_threshold {
            layer.set(x, y, @types.Floor)
          }
        } else {
          if walls >= survival_threshold {
            layer.set(x, y, @types.Wall)
          }
        }
      }
    }
  }
}

///|
pub fn generate(
  config : LayeredCavesConfig,
  rng : @types.RNG
) -> LayeredCavesResult {
  let grid = @types.Grid3D::make(
    config.width, config.height, config.depth, @types.Wall,
  )
  // Generate cave on each layer
  for z in 0..<config.depth {
    let layer = grid.get_layer(z)
    generate_cave_layer(layer, config.width, config.height, rng)
  }
  // Connect layers by placing Door tiles where both adjacent layers have Floor
  for z in 0..<(config.depth - 1) {
    // Collect candidate positions where both layers are floor
    let candidates : Array[@types.Point] = []
    for y in 1..<(config.height - 1) {
      for x in 1..<(config.width - 1) {
        if grid.get(x, y, z) == @types.Floor &&
          grid.get(x, y, z + 1) == @types.Floor {
          candidates.push({ x, y })
        }
      }
    }
    if candidates.length() == 0 {
      // Fallback: carve a connection at center
      let cx = config.width / 2
      let cy = config.height / 2
      grid.set(cx, cy, z, @types.Door)
      grid.set(cx, cy, z + 1, @types.Door)
    } else {
      // Place Door tiles at a fraction of candidate positions
      let num_connections = {
        let n = (candidates.length().to_double() * config.connectivity).to_int()
        if n < 1 { 1 } else { n }
      }
      rng.rand_shuffle(candidates)
      let limit = if num_connections < candidates.length() {
        num_connections
      } else {
        candidates.length()
      }
      for i in 0..<limit {
        let p = candidates[i]
        grid.set(p.x, p.y, z, @types.Door)
        grid.set(p.x, p.y, z + 1, @types.Door)
      }
    }
  }
  // Count open cells (Floor or Door)
  let mut open_cells = 0
  for z in 0..<config.depth {
    for y in 0..<config.height {
      for x in 0..<config.width {
        let tile = grid.get(x, y, z)
        if tile == @types.Floor || tile == @types.Door {
          open_cells += 1
        }
      }
    }
  }
  { grid, open_cells }
}
