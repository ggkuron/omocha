///|
pub(all) struct TerracedMinesConfig {
  width : Int
  height : Int
  max_height : Int
  num_terraces : Int
  ore_chance : Double
}

///|
pub fn TerracedMinesConfig::default() -> TerracedMinesConfig {
  { width: 48, height: 48, max_height: 8, num_terraces: 5, ore_chance: 0.1 }
}

///|
pub(all) struct TerracedMinesResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : TerracedMinesConfig,
  rng : @types.RNG
) -> TerracedMinesResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Divide the map into horizontal terrace bands
  // Each terrace is a flat area at a certain height
  // Heights descend from top (entrance) to bottom (deepest mine level)
  let terrace_height_step = if config.num_terraces > 1 {
    config.max_height / (config.num_terraces - 1)
  } else {
    0
  }
  let band_h = (h - 2) / config.num_terraces // height of each terrace band
  for terrace_idx in 0..<config.num_terraces {
    let band_y = 1 + terrace_idx * band_h
    let band_end_y = if terrace_idx == config.num_terraces - 1 {
      h - 1
    } else {
      band_y + band_h
    }
    // Terrace height: descending (top = highest, bottom = lowest)
    let terrace_h = config.max_height - terrace_idx * terrace_height_step
    let actual_h = terrace_h.max(0)
    // Carve the terrace floor area with some irregularity
    let margin_x = rng.rand_range(2, 5)
    let terrace_x_start = margin_x
    let terrace_x_end = w - margin_x
    let mut min_x = w
    let mut min_y = h
    let mut max_x = 0
    let mut max_y = 0
    for y in band_y..<band_end_y {
      // Add some horizontal randomness to edges
      let left_offset = rng.rand_range(0, 3)
      let right_offset = rng.rand_range(0, 3)
      let row_start = (terrace_x_start + left_offset).min(w - 2)
      let row_end = (terrace_x_end - right_offset).max(row_start + 1)
      for x in row_start..<row_end {
        if grid.in_bounds(x, y) {
          grid.set(x, y, @types.Floor)
          grid.set_height(x, y, actual_h)
          if x < min_x {
            min_x = x
          }
          if y < min_y {
            min_y = y
          }
          if x > max_x {
            max_x = x
          }
          if y > max_y {
            max_y = y
          }
        }
      }
    }
    if max_x >= min_x && max_y >= min_y {
      rooms.push(
        {
          rect: {
            x: min_x,
            y: min_y,
            w: max_x - min_x + 1,
            h: max_y - min_y + 1,
          },
          id: room_id,
        },
      )
      room_id += 1
    }
    // Add ore deposits (marked as Key tiles)
    for y in (band_y + 1)..<(band_end_y - 1) {
      for x in (terrace_x_start + 1)..<(terrace_x_end - 1) {
        if grid.in_bounds(x, y) &&
          grid.get(x, y) == @types.Floor &&
          rng.rand_bool(config.ore_chance) {
          grid.set(x, y, @types.Key)
        }
      }
    }
    // Add support pillars (Wall cells) inside the terrace
    let num_pillars = rng.rand_range(1, 4)
    for _ in 0..<num_pillars {
      let px = rng.rand_range(terrace_x_start + 2, terrace_x_end - 2)
      let py = rng.rand_range(band_y + 1, band_end_y - 1)
      if grid.in_bounds(px, py) && grid.get(px, py) == @types.Floor {
        grid.set(px, py, @types.Wall)
        grid.set_height(px, py, actual_h + 1)
      }
    }
  }
  // Connect terraces with corridors (mine shafts)
  // Each corridor connects adjacent terrace levels
  for i in 0..<(config.num_terraces - 1) {
    let upper_band_y = 1 + i * band_h
    let lower_band_y = 1 + (i + 1) * band_h
    let upper_h = (config.max_height - i * terrace_height_step).max(0)
    let lower_h = (config.max_height - (i + 1) * terrace_height_step).max(0)
    // Create 1-2 connecting shafts per terrace transition
    let num_shafts = rng.rand_range(1, 3)
    for _ in 0..<num_shafts {
      let shaft_x = rng.rand_range(4, w - 4)
      // Carve shaft from upper terrace bottom edge to lower terrace top edge
      let shaft_start_y = upper_band_y + band_h - 1
      let shaft_end_y = lower_band_y + 1
      let mut cur_h = upper_h
      for y in shaft_start_y..=shaft_end_y {
        if grid.in_bounds(shaft_x, y) {
          // Gradually decrease height
          let t = if shaft_end_y > shaft_start_y {
            (y - shaft_start_y).to_double() /
            (shaft_end_y - shaft_start_y).to_double()
          } else {
            1.0
          }
          cur_h = upper_h - ((upper_h - lower_h).to_double() * t).to_int()
          grid.set(shaft_x, y, @types.Corridor)
          grid.set_height(shaft_x, y, cur_h)
          // Widen shaft slightly
          if grid.in_bounds(shaft_x - 1, y) {
            grid.set(shaft_x - 1, y, @types.Corridor)
            grid.set_height(shaft_x - 1, y, cur_h)
          }
        }
      }
    }
  }
  // Add mine cart tracks (corridor tiles) along terrace floors
  for terrace_idx in 0..<config.num_terraces {
    let band_y_start = 1 + terrace_idx * band_h
    let track_y = band_y_start + band_h / 2
    if track_y < h - 1 {
      for x in 3..<(w - 3) {
        if grid.in_bounds(x, track_y) && grid.get(x, track_y) == @types.Floor {
          grid.set(x, track_y, @types.Corridor)
        }
      }
    }
  }
  // Add wall edges along terrace boundaries where height changes
  add_terrace_walls(grid, w, h)
  // Place Start at top terrace, Goal at bottom terrace
  place_mine_start_goal(grid, w, h, config)
  { grid, rooms }
}

///|
fn add_terrace_walls(grid : @types.HeightGrid, w : Int, h : Int) -> Unit {
  let edges : Array[(Int, Int, Int)] = [] // (x, y, wall_h)
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let tile = grid.get(x, y)
      if tile == @types.Wall {
        continue
      }
      let cur_h = grid.get_height(x, y)
      for d in dirs {
        let nx = x + d.0
        let ny = y + d.1
        if grid.in_bounds(nx, ny) && grid.get(nx, ny) == @types.Wall &&
          grid.get_height(nx, ny) == 0 {
          // Wall cell adjacent to a floor cell: raise to match
          edges.push((nx, ny, cur_h))
        }
      }
    }
  }
  for e in edges {
    grid.set_height(e.0, e.1, e.2)
  }
}

///|
fn place_mine_start_goal(
  grid : @types.HeightGrid,
  w : Int,
  h : Int,
  _config : TerracedMinesConfig
) -> Unit {
  // Start: top area (entrance)
  let mut start_placed = false
  for y in 1..<(h / 3) {
    for x in 1..<(w - 1) {
      if not(start_placed) {
        let tile = grid.get(x, y)
        if tile == @types.Floor || tile == @types.Corridor {
          grid.set(x, y, @types.Start)
          start_placed = true
        }
      }
    }
  }
  // Goal: bottom area (deepest level)
  let mut goal_placed = false
  for y = h - 2; y > h * 2 / 3; y = y - 1 {
    for x = w - 2; x > 0; x = x - 1 {
      if not(goal_placed) {
        let tile = grid.get(x, y)
        if tile == @types.Floor || tile == @types.Corridor {
          grid.set(x, y, @types.Goal)
          goal_placed = true
        }
      }
    }
  }
  // Fallback goal
  if not(goal_placed) {
    for y = h - 2; y > 0; y = y - 1 {
      for x = w - 2; x > 0; x = x - 1 {
        if not(goal_placed) {
          let tile = grid.get(x, y)
          if tile == @types.Floor || tile == @types.Corridor {
            grid.set(x, y, @types.Goal)
            goal_placed = true
          }
        }
      }
    }
  }
}
