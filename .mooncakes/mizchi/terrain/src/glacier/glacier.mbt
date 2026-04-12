///|
pub(all) struct GlacierConfig {
  width : Int
  height : Int
  max_height : Int
  num_crevasses : Int
  cave_chance : Double
}

///|
pub fn GlacierConfig::default() -> GlacierConfig {
  { width: 48, height: 30, max_height: 8, num_crevasses: 5, cave_chance: 0.3 }
}

///|
pub(all) struct GlacierResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : GlacierConfig, rng : @types.RNG) -> GlacierResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Generate ice sheet: high elevation that gradually descends from top to bottom
  // with some lateral variation
  for y in 0..<h {
    for x in 0..<w {
      // Base height: descends from top (north) to bottom (south)
      let y_factor = 1.0 - y.to_double() / (h - 1).to_double()
      // Lateral wave for natural variation
      let x_wave = (sin(x.to_double() * 0.15) * 1.5).to_int()
      let base_h = (y_factor * config.max_height.to_double()).to_int() + x_wave
      // Add small noise
      let noise = rng.rand_range(-1, 2)
      let final_h = (base_h + noise).max(1).min(config.max_height)
      grid.set(x, y, @types.Wall)
      grid.set_height(x, y, final_h)
    }
  }
  // Smooth the ice surface
  let temp_heights = Array::make(w * h, 0)
  for y in 0..<h {
    for x in 0..<w {
      temp_heights[y * w + x] = grid.get_height(x, y)
    }
  }
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let avg = (temp_heights[(y - 1) * w + x] +
        temp_heights[(y + 1) * w + x] +
        temp_heights[y * w + (x - 1)] +
        temp_heights[y * w + (x + 1)] +
        temp_heights[y * w + x]) /
        5
      grid.set_height(x, y, avg.max(1).min(config.max_height))
    }
  }
  // Carve crevasses (deep narrow cracks in the ice)
  for _i in 0..<config.num_crevasses {
    // Start point: random location, biased toward middle
    let cx = rng.rand_range(3, w - 3)
    let cy = rng.rand_range(2, h - 2)
    // Crevasse direction: mostly vertical (north-south) with drift
    let length = rng.rand_range(8, h / 2)
    let drift_dir = if rng.rand_bool(0.5) { 1 } else { -1 }
    let mut x = cx
    let mut y = cy
    for _step in 0..<length {
      if x < 1 || x >= w - 1 || y < 1 || y >= h - 1 {
        break
      }
      // Crevasse is 1-2 cells wide, very low height
      let crev_width = rng.rand_range(1, 3)
      for dx in 0..<crev_width {
        let px = x + dx
        if px >= 0 && px < w {
          grid.set(px, y, @types.Floor)
          grid.set_height(px, y, 0)
        }
      }
      // Move mostly downward with occasional lateral drift
      y += 1
      if rng.rand_bool(0.3) {
        x += drift_dir
      }
      // Occasional fork
      if rng.rand_bool(0.05) {
        // Carve a short branch
        let branch_len = rng.rand_range(2, 5)
        let mut bx = x
        let by_dir = rng.rand_range(-1, 2)
        for _b in 0..<branch_len {
          bx += drift_dir
          let by2 = y + by_dir * _b
          if bx >= 0 && bx < w && by2 >= 0 && by2 < h {
            grid.set(bx, by2, @types.Floor)
            grid.set_height(bx, by2, 0)
          }
        }
      }
    }
  }
  // Carve ice caves (rooms inside the glacier)
  for y_scan in 2..<(h - 4) {
    for x_scan in 2..<(w - 4) {
      // Only create caves in high-elevation areas
      let local_h = grid.get_height(x_scan, y_scan)
      if local_h >= config.max_height / 2 && rng.rand_bool(config.cave_chance * 0.05) {
        let cave_w = rng.rand_range(3, 6)
        let cave_h = rng.rand_range(3, 5)
        if x_scan + cave_w < w - 1 && y_scan + cave_h < h - 1 {
          // Check if area is all high ice
          let mut all_high = true
          for cy in y_scan..<(y_scan + cave_h) {
            for cx in x_scan..<(x_scan + cave_w) {
              if grid.get_height(cx, cy) < config.max_height / 2 {
                all_high = false
                break
              }
            }
            if not(all_high) {
              break
            }
          }
          if all_high {
            // Carve cave interior
            let cave_floor_h = local_h / 2
            for cy in y_scan..<(y_scan + cave_h) {
              for cx in x_scan..<(x_scan + cave_w) {
                let is_border = cx == x_scan || cx == x_scan + cave_w - 1 ||
                  cy == y_scan || cy == y_scan + cave_h - 1
                if is_border {
                  grid.set(cx, cy, @types.Wall)
                  grid.set_height(cx, cy, cave_floor_h + 1)
                } else {
                  grid.set(cx, cy, @types.Floor)
                  grid.set_height(cx, cy, cave_floor_h)
                }
              }
            }
            // Add door entrance
            let door_x = x_scan + cave_w / 2
            let door_y = y_scan + cave_h - 1
            if door_x >= 0 && door_x < w && door_y >= 0 && door_y < h {
              grid.set(door_x, door_y, @types.Door)
              grid.set_height(door_x, door_y, cave_floor_h)
            }
            rooms.push(
              {
                rect: {
                  x: x_scan,
                  y: y_scan,
                  w: cave_w,
                  h: cave_h,
                },
                id: room_id,
              },
            )
            room_id += 1
          }
        }
      }
    }
  }
  // Add meltwater pools at low-elevation areas (south/bottom)
  for y in (h * 2 / 3)..<h {
    for x in 0..<w {
      let cur_h = grid.get_height(x, y)
      let cur_tile = grid.get(x, y)
      if cur_h <= 2 && cur_tile == @types.Wall {
        // Low ice melts into water pools
        if rng.rand_bool(0.4) {
          grid.set(x, y, @types.Water)
          grid.set_height(x, y, 0)
        }
      }
    }
  }
  // Expand water pools slightly for more natural appearance
  let water_positions : Array[@types.Point] = []
  for y in 0..<h {
    for x in 0..<w {
      if grid.get(x, y) == @types.Water {
        water_positions.push({ x, y })
      }
    }
  }
  for wp in water_positions {
    let dirs : Array[(Int, Int)] = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    for d in dirs {
      let nx = wp.x + d.0
      let ny = wp.y + d.1
      if nx >= 0 && nx < w && ny >= 0 && ny < h {
        if grid.get(nx, ny) == @types.Wall && grid.get_height(nx, ny) <= 2 {
          if rng.rand_bool(0.3) {
            grid.set(nx, ny, @types.Water)
            grid.set_height(nx, ny, 0)
          }
        }
      }
    }
  }
  // Add seracs (tall ice pillars at max height) in upper portion
  let num_seracs = rng.rand_range(3, 8)
  for _i in 0..<num_seracs {
    let sx = rng.rand_range(2, w - 2)
    let sy = rng.rand_range(1, h / 3)
    // Place a 1x1 or 2x2 serac
    let serac_size = rng.rand_range(1, 3)
    for dy in 0..<serac_size {
      for dx in 0..<serac_size {
        let px = sx + dx
        let py = sy + dy
        if px >= 0 && px < w && py >= 0 && py < h {
          grid.set(px, py, @types.Wall)
          grid.set_height(px, py, config.max_height)
        }
      }
    }
  }
  // Place Start at top-left accessible area, Goal at bottom-right
  let mut start_placed = false
  for y in 0..<h {
    for x in 0..<w {
      if not(start_placed) {
        let tile = grid.get(x, y)
        if tile == @types.Floor || tile == @types.Corridor {
          grid.set(x, y, @types.Start)
          start_placed = true
        }
      }
    }
  }
  // If no floor found, place on a low wall cell
  if not(start_placed) {
    grid.set(1, 1, @types.Start)
    grid.set_height(1, 1, grid.get_height(1, 1))
  }
  let mut goal_placed = false
  for y = h - 1; y >= 0; y = y - 1 {
    for x = w - 1; x >= 0; x = x - 1 {
      if not(goal_placed) {
        let tile = grid.get(x, y)
        if tile == @types.Floor || tile == @types.Water {
          grid.set(x, y, @types.Goal)
          goal_placed = true
        }
      }
    }
  }
  if not(goal_placed) {
    grid.set(w - 2, h - 2, @types.Goal)
  }
  { grid, rooms }
}

///|
fn sin(x : Double) -> Double {
  // Taylor series approximation for sin
  // Normalize to [-pi, pi]
  let pi = 3.14159265358979
  let mut v = x
  while v > pi {
    v = v - 2.0 * pi
  }
  while v < -pi {
    v = v + 2.0 * pi
  }
  let x2 = v * v
  let x3 = v * x2
  let x5 = x3 * x2
  let x7 = x5 * x2
  v - x3 / 6.0 + x5 / 120.0 - x7 / 5040.0
}
