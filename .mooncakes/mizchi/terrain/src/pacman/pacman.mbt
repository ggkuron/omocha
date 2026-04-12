///|
pub(all) struct PacmanConfig {
  width : Int
  height : Int
  wall_density : Double
  corridor_width : Int
  chamber_size : Int
  symmetry : Int
}

///|
pub fn PacmanConfig::default() -> PacmanConfig {
  {
    width: 50,
    height: 25,
    wall_density: 0.3,
    corridor_width: 2,
    chamber_size: 5,
    symmetry: 0,
  }
}

///|
pub(all) struct PacmanResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn count_wall_neighbors(grid : @types.Grid2D, x : Int, y : Int) -> Int {
  let mut count = 0
  for dy in -1..=1 {
    for dx in -1..=1 {
      if dx == 0 && dy == 0 {
        continue
      }
      let nx = x + dx
      let ny = y + dy
      if not(grid.in_bounds(nx, ny)) || grid.get(nx, ny) == @types.Wall {
        count += 1
      }
    }
  }
  count
}

///|
pub fn generate(config : PacmanConfig, rng : @types.RNG) -> PacmanResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Floor)
  let rooms : Array[@types.Room] = []
  // Fill borders with walls
  for x in 0..<config.width {
    grid.set(x, 0, @types.Wall)
    grid.set(x, config.height - 1, @types.Wall)
  }
  for y in 0..<config.height {
    grid.set(0, y, @types.Wall)
    grid.set(config.width - 1, y, @types.Wall)
  }
  if config.symmetry == 0 {
    // Vertical mirror (left-right symmetry)
    let half_w = config.width / 2
    // Step 1: Random wall placement on the left half
    for y in 1..<(config.height - 1) {
      for x in 1..<half_w {
        if rng.rand_bool(config.wall_density) {
          grid.set(x, y, @types.Wall)
        }
      }
    }
    // Step 2: Cellular automata smoothing on left half (3 iterations)
    for _ in 0..<3 {
      let old = grid.clone()
      for y in 1..<(config.height - 1) {
        for x in 1..<half_w {
          let walls = count_wall_neighbors(old, x, y)
          if old.get(x, y) == @types.Wall {
            // Wall stays if it has enough wall neighbors
            if walls < 3 {
              grid.set(x, y, @types.Floor)
            }
          } else {
            // Floor becomes wall if surrounded by too many walls
            if walls >= 5 {
              grid.set(x, y, @types.Wall)
            }
          }
        }
      }
    }
    // Step 3: Enforce minimum corridor width on left half
    // Remove isolated wall pillars that would block corridors
    for y in 1..<(config.height - 1) {
      for x in 1..<half_w {
        if grid.get(x, y) == @types.Wall {
          // Check if this wall creates a corridor that's too narrow
          let mut narrow = false
          // Check horizontal narrowing
          for cw in 1..<config.corridor_width {
            if grid.in_bounds(x, y - cw) &&
              grid.in_bounds(x, y + cw) &&
              grid.get(x, y - cw) == @types.Wall &&
              grid.get(x, y + cw) == @types.Wall {
              narrow = true
            }
          }
          if narrow {
            grid.set(x, y, @types.Floor)
          }
        }
      }
    }
    // Step 4: Mirror left half to right half
    for y in 1..<(config.height - 1) {
      for x in 1..<half_w {
        let mirror_x = config.width - 1 - x
        if grid.in_bounds(mirror_x, y) {
          grid.set(mirror_x, y, grid.get(x, y))
        }
      }
    }
  } else {
    // Horizontal mirror (top-bottom symmetry)
    let half_h = config.height / 2
    // Step 1: Random wall placement on the top half
    for y in 1..<half_h {
      for x in 1..<(config.width - 1) {
        if rng.rand_bool(config.wall_density) {
          grid.set(x, y, @types.Wall)
        }
      }
    }
    // Step 2: Cellular automata smoothing on top half
    for _ in 0..<3 {
      let old = grid.clone()
      for y in 1..<half_h {
        for x in 1..<(config.width - 1) {
          let walls = count_wall_neighbors(old, x, y)
          if old.get(x, y) == @types.Wall {
            if walls < 3 {
              grid.set(x, y, @types.Floor)
            }
          } else {
            if walls >= 5 {
              grid.set(x, y, @types.Wall)
            }
          }
        }
      }
    }
    // Step 3: Enforce minimum corridor width on top half
    for y in 1..<half_h {
      for x in 1..<(config.width - 1) {
        if grid.get(x, y) == @types.Wall {
          let mut narrow = false
          for cw in 1..<config.corridor_width {
            if grid.in_bounds(x - cw, y) &&
              grid.in_bounds(x + cw, y) &&
              grid.get(x - cw, y) == @types.Wall &&
              grid.get(x + cw, y) == @types.Wall {
              narrow = true
            }
          }
          if narrow {
            grid.set(x, y, @types.Floor)
          }
        }
      }
    }
    // Step 4: Mirror top half to bottom half
    for y in 1..<half_h {
      for x in 1..<(config.width - 1) {
        let mirror_y = config.height - 1 - y
        if grid.in_bounds(x, mirror_y) {
          grid.set(x, mirror_y, grid.get(x, y))
        }
      }
    }
  }
  // Step 5: Carve central chamber
  let cx = config.width / 2
  let cy = config.height / 2
  let half_chamber = config.chamber_size / 2
  let chamber_rect : @types.Rect = {
    x: cx - half_chamber,
    y: cy - half_chamber,
    w: config.chamber_size,
    h: config.chamber_size,
  }
  grid.fill_rect(chamber_rect, @types.Floor)
  rooms.push({ rect: chamber_rect, id: 0 })
  // Add openings from chamber to the corridors (4 cardinal directions)
  // Top opening
  for x in (cx - 1)..=(cx + 1) {
    for y in (cy - half_chamber - 1)..<(cy - half_chamber) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Bottom opening
  for x in (cx - 1)..=(cx + 1) {
    for y in (cy + half_chamber + 1)..<(cy + half_chamber + 2) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Left opening
  for y in (cy - 1)..=(cy + 1) {
    for x in (cx - half_chamber - 1)..<(cx - half_chamber) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Right opening
  for y in (cy - 1)..=(cy + 1) {
    for x in (cx + half_chamber + 1)..<(cx + half_chamber + 2) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Identify and record rooms formed by the maze structure
  // Scan for rectangular floor regions as additional rooms
  let visited = @types.Grid2D::make(config.width, config.height, @types.Empty)
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if grid.get(x, y) == @types.Floor && visited.get(x, y) == @types.Empty {
        // Try to find a rectangular region starting here
        let mut max_w = 0
        while x + max_w < config.width - 1 &&
          grid.get(x + max_w, y) == @types.Floor &&
          visited.get(x + max_w, y) == @types.Empty {
          max_w += 1
        }
        let mut max_h = 1
        let mut valid = true
        while valid && y + max_h < config.height - 1 && max_h < 6 {
          for rx in x..<(x + max_w) {
            if grid.get(rx, y + max_h) != @types.Floor ||
              visited.get(rx, y + max_h) != @types.Empty {
              valid = false
              break
            }
          }
          if valid {
            max_h += 1
          }
        }
        // Only record if it's a meaningful room (not just a corridor)
        if max_w >= 3 && max_h >= 3 {
          let rect : @types.Rect = { x, y, w: max_w, h: max_h }
          rooms.push({ rect, id: rooms.length() })
          // Mark as visited
          for vy in y..<(y + max_h) {
            for vx in x..<(x + max_w) {
              visited.set(vx, vy, @types.Wall)
            }
          }
        }
      }
    }
  }
  // Place Start at left side, center height
  grid.set(1, cy, @types.Start)
  // Place Goal at right side, center height
  grid.set(config.width - 2, cy, @types.Goal)
  { grid, rooms }
}
