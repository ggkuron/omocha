///|
pub(all) struct LayeredPlateauConfig {
  width : Int
  height : Int
  max_height : Int
  num_plateaus : Int
  erosion : Double
}

///|
pub fn LayeredPlateauConfig::default() -> LayeredPlateauConfig {
  { width: 48, height: 48, max_height: 6, num_plateaus: 4, erosion: 0.3 }
}

///|
pub(all) struct LayeredPlateauResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : LayeredPlateauConfig,
  rng : @types.RNG
) -> LayeredPlateauResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Floor, config.max_height)
  // Center of the map
  let cx = w / 2
  let cy = h / 2
  // Calculate max radius for the outermost plateau
  let max_radius_x = (w / 2) - 2
  let max_radius_y = (h / 2) - 2
  let max_radius = max_radius_x.min(max_radius_y)
  // Create concentric plateaus from outermost (lowest) to innermost (highest)
  // Outermost layer is height 0 (already set), each inner layer is higher
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Set border walls
  for x in 0..<w {
    grid.set(x, 0, @types.Wall)
    grid.set(x, h - 1, @types.Wall)
  }
  for y in 0..<h {
    grid.set(0, y, @types.Wall)
    grid.set(w - 1, y, @types.Wall)
  }
  // Generate plateaus from outer to inner
  for layer in 0..<config.num_plateaus {
    // Radius decreases for inner plateaus
    let t = (layer + 1).to_double() / config.num_plateaus.to_double()
    let radius = max_radius.to_double() * (1.0 - t * 0.7)
    let radius_int = radius.to_int()
    // Height increases for inner plateaus
    let layer_height = ((layer + 1).to_double() /
      config.num_plateaus.to_double() *
      config.max_height.to_double())
      .to_int()
      .min(config.max_height)
    // Track bounds for room creation
    let mut min_x = w
    let mut min_y = h
    let mut max_x = 0
    let mut max_y = 0
    // Fill elliptical area
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let dx = x - cx
        let dy = y - cy
        let dist_sq = dx * dx + dy * dy
        let r_sq = radius_int * radius_int
        if dist_sq <= r_sq {
          grid.set(x, y, @types.Floor)
          grid.set_height(x, y, layer_height)
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
  }
  // Add walls at plateau edges (where height changes)
  mark_plateau_edges(grid, w, h)
  // Apply erosion: randomly remove some edge cells
  apply_erosion(grid, config, rng)
  // Carve paths between plateau levels (ramps)
  carve_ramps(grid, config, rng)
  // Place Start at lowest plateau edge, Goal at highest plateau center
  place_start_goal(grid, w, h, config)
  { grid, rooms }
}

///|
fn mark_plateau_edges(grid : @types.HeightGrid, w : Int, h : Int) -> Unit {
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  // Collect edge positions first to avoid modifying while iterating
  let edges : Array[(Int, Int)] = []
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if grid.get(x, y) == @types.Floor {
        let cur_h = grid.get_height(x, y)
        let mut is_edge = false
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) {
            let neighbor_h = grid.get_height(nx, ny)
            let neighbor_tile = grid.get(nx, ny)
            if neighbor_tile == @types.Floor && abs(cur_h - neighbor_h) >= 2 {
              is_edge = true
            }
          }
        }
        if is_edge {
          edges.push((x, y))
        }
      }
    }
  }
  for e in edges {
    let cur_h = grid.get_height(e.0, e.1)
    grid.set(e.0, e.1, @types.Wall)
    grid.set_height(e.0, e.1, cur_h)
  }
}

///|
fn apply_erosion(
  grid : @types.HeightGrid,
  config : LayeredPlateauConfig,
  rng : @types.RNG
) -> Unit {
  let w = config.width
  let h = config.height
  let dirs : Array[(Int, Int)] = [
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
  ]
  // Erode edges of plateaus
  for y in 2..<(h - 2) {
    for x in 2..<(w - 2) {
      if grid.get(x, y) == @types.Floor && grid.get_height(x, y) > 0 {
        // Count neighbors at same height
        let cur_h = grid.get_height(x, y)
        let mut same_count = 0
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) &&
            grid.get(nx, ny) == @types.Floor &&
            grid.get_height(nx, ny) == cur_h {
            same_count += 1
          }
        }
        // Edge cells (fewer same-height neighbors) are candidates for erosion
        if same_count < 5 && rng.rand_bool(config.erosion) {
          // Lower this cell by one level
          let new_h = (cur_h - 1).max(0)
          grid.set_height(x, y, new_h)
        }
      }
    }
  }
}

///|
fn carve_ramps(
  grid : @types.HeightGrid,
  config : LayeredPlateauConfig,
  rng : @types.RNG
) -> Unit {
  let w = config.width
  let h = config.height
  let cx = w / 2
  let cy = h / 2
  // Create ramps in 4 cardinal directions from center
  let directions : Array[(Int, Int)] = [(1, 0), (-1, 0), (0, 1), (0, -1)]
  for dir in directions {
    if rng.rand_bool(0.7) {
      // 70% chance to create a ramp in each direction
      let mut x = cx
      let mut y = cy
      let mut prev_h = grid.get_height(cx, cy)
      // Walk outward, creating smooth height transitions
      while grid.in_bounds(x, y) && x > 0 && x < w - 1 && y > 0 && y < h - 1 {
        let cur_h = grid.get_height(x, y)
        if cur_h < prev_h {
          // Smooth the transition
          grid.set(x, y, @types.Corridor)
          grid.set_height(x, y, prev_h - 1)
          prev_h = prev_h - 1
        } else {
          prev_h = cur_h
        }
        x += dir.0
        y += dir.1
      }
    }
  }
}

///|
fn place_start_goal(
  grid : @types.HeightGrid,
  w : Int,
  h : Int,
  config : LayeredPlateauConfig
) -> Unit {
  ignore(config)
  // Start: first walkable cell near top-left
  let mut start_placed = false
  for y in 1..<(h - 1) {
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
  // Goal: center of the highest plateau
  let mut goal_placed = false
  let mut best_h = -1
  let mut best_x = w / 2
  let mut best_y = h / 2
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let tile = grid.get(x, y)
      if (tile == @types.Floor || tile == @types.Corridor) &&
        grid.get_height(x, y) > best_h {
        best_h = grid.get_height(x, y)
        best_x = x
        best_y = y
      }
    }
  }
  if grid.get(best_x, best_y) != @types.Start {
    grid.set(best_x, best_y, @types.Goal)
    goal_placed = true
  }
  if not(goal_placed) {
    // Fallback: find any floor cell
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

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}
