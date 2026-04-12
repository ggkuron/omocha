///|
pub(all) struct CityBlockConfig {
  width : Int
  height : Int
  block_size : Int
  street_width : Int
  building_chance : Double
  door_chance : Double
  remove_street_chance : Double
}

///|
pub fn CityBlockConfig::default() -> CityBlockConfig {
  {
    width: 50,
    height: 25,
    block_size: 6,
    street_width: 1,
    building_chance: 0.7,
    door_chance: 0.5,
    remove_street_chance: 0.15,
  }
}

///|
pub(all) struct CityBlockResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn lay_streets(
  grid : @types.Grid2D,
  config : CityBlockConfig,
  rng : @types.RNG
) -> Unit {
  let stride = config.block_size + config.street_width
  // Lay horizontal streets
  let mut y = 0
  while y < config.height {
    // Check if this is a street row
    if y % stride < config.street_width {
      for x in 0..<config.width {
        // Randomly remove some street segments
        if not(rng.rand_bool(config.remove_street_chance)) {
          for sw in 0..<config.street_width {
            if y + sw < config.height {
              grid.set(x, y + sw, @types.Corridor)
            }
          }
        }
      }
    }
    y += 1
  }
  // Lay vertical streets
  let mut x = 0
  while x < config.width {
    if x % stride < config.street_width {
      for y in 0..<config.height {
        if not(rng.rand_bool(config.remove_street_chance)) {
          for sw in 0..<config.street_width {
            if x + sw < config.width {
              grid.set(x + sw, y, @types.Corridor)
            }
          }
        }
      }
    }
    x += 1
  }
}

///|
fn fill_blocks(
  grid : @types.Grid2D,
  config : CityBlockConfig,
  rng : @types.RNG,
  rooms : Array[@types.Room]
) -> Unit {
  let stride = config.block_size + config.street_width
  // Iterate over each block
  let mut by = config.street_width
  while by + config.block_size <= config.height {
    let mut bx = config.street_width
    while bx + config.block_size <= config.width {
      // Check if this position is aligned with the block grid
      if by % stride == config.street_width && bx % stride == config.street_width {
        if rng.rand_bool(config.building_chance) {
          // Fill block with Floor (building)
          let rect : @types.Rect = {
            x: bx,
            y: by,
            w: config.block_size,
            h: config.block_size,
          }
          grid.fill_rect(rect, @types.Floor)
          let room : @types.Room = { rect, id: rooms.length() }
          rooms.push(room)
          // Add doors on edges adjacent to streets
          add_doors(grid, config, rng, bx, by)
        } else {
          // Empty lot: fill with Wall
          for dy in 0..<config.block_size {
            for dx in 0..<config.block_size {
              let px = bx + dx
              let py = by + dy
              if grid.in_bounds(px, py) {
                grid.set(px, py, @types.Wall)
              }
            }
          }
        }
      }
      bx += stride
    }
    by += stride
  }
}

///|
fn add_doors(
  grid : @types.Grid2D,
  config : CityBlockConfig,
  rng : @types.RNG,
  bx : Int,
  by : Int
) -> Unit {
  let bs = config.block_size
  // Try to add a door on each of the 4 sides
  // Top edge
  if by > 0 && rng.rand_bool(config.door_chance) {
    let dx = rng.rand_range(0, bs)
    let px = bx + dx
    let py = by
    if grid.in_bounds(px, py) {
      grid.set(px, py, @types.Door)
    }
  }
  // Bottom edge
  if by + bs < config.height && rng.rand_bool(config.door_chance) {
    let dx = rng.rand_range(0, bs)
    let px = bx + dx
    let py = by + bs - 1
    if grid.in_bounds(px, py) {
      grid.set(px, py, @types.Door)
    }
  }
  // Left edge
  if bx > 0 && rng.rand_bool(config.door_chance) {
    let dy = rng.rand_range(0, bs)
    let px = bx
    let py = by + dy
    if grid.in_bounds(px, py) {
      grid.set(px, py, @types.Door)
    }
  }
  // Right edge
  if bx + bs < config.width && rng.rand_bool(config.door_chance) {
    let dy = rng.rand_range(0, bs)
    let px = bx + bs - 1
    let py = by + dy
    if grid.in_bounds(px, py) {
      grid.set(px, py, @types.Door)
    }
  }
}

///|
pub fn generate(
  config : CityBlockConfig,
  rng : @types.RNG
) -> CityBlockResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // First lay down the street grid
  lay_streets(grid, config, rng)
  // Then fill in the blocks
  fill_blocks(grid, config, rng, rooms)
  { grid, rooms }
}
