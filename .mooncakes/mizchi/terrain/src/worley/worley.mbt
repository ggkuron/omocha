///|
pub(all) struct WorleyConfig {
  width : Int
  height : Int
  num_points : Int
  threshold : Double
  use_manhattan : Int
}

///|
pub fn WorleyConfig::default() -> WorleyConfig {
  { width: 50, height: 25, num_points: 20, threshold: 0.3, use_manhattan: 1 }
}

///|
pub(all) struct WorleyResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  open_cells : Int
}

///|
pub fn generate(config : WorleyConfig, rng : @types.RNG) -> WorleyResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Place random feature points
  let points_x = Array::make(config.num_points, 0.0)
  let points_y = Array::make(config.num_points, 0.0)
  for i in 0..<config.num_points {
    points_x[i] = rng.rand_range(1, config.width - 1).to_double()
    points_y[i] = rng.rand_range(1, config.height - 1).to_double()
  }
  // Compute max possible distance for normalization
  let max_possible = if config.use_manhattan == 1 {
    (config.width + config.height).to_double()
  } else {
    (config.width * config.width + config.height * config.height)
      .to_double()
      .sqrt()
  }
  let mut open_cells = 0
  // For each cell, compute F1 and F2 distances
  for y in 0..<config.height {
    for x in 0..<config.width {
      // Border is always wall
      if x == 0 || y == 0 || x == config.width - 1 || y == config.height - 1 {
        continue
      }
      let px = x.to_double()
      let py = y.to_double()
      let mut f1 = max_possible
      let mut f2 = max_possible
      for i in 0..<config.num_points {
        let dist = if config.use_manhattan == 1 {
          abs_double(px - points_x[i]) + abs_double(py - points_y[i])
        } else {
          let dx = px - points_x[i]
          let dy = py - points_y[i]
          (dx * dx + dy * dy).sqrt()
        }
        if dist < f1 {
          f2 = f1
          f1 = dist
        } else if dist < f2 {
          f2 = dist
        }
      }
      // Normalize the difference
      let value = (f2 - f1) / max_possible
      if value < config.threshold {
        grid.set(x, y, @types.Floor)
        open_cells += 1
      }
    }
  }
  // Identify rooms via flood fill on connected floor regions
  let rooms = find_rooms(grid, config.width, config.height)
  // Place Start and Goal
  if open_cells > 0 {
    place_start_goal(grid, config.width, config.height)
  }
  { grid, rooms, open_cells }
}

///|
fn abs_double(x : Double) -> Double {
  if x < 0.0 {
    -x
  } else {
    x
  }
}

///|
fn abs_int(x : Int) -> Int {
  if x < 0 {
    -x
  } else {
    x
  }
}

///|
fn find_rooms(
  grid : @types.Grid2D,
  width : Int,
  height : Int
) -> Array[@types.Room] {
  let visited = Array::make(width * height, false)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for y in 1..<(height - 1) {
    for x in 1..<(width - 1) {
      let idx = y * width + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        let comp = @types.flood_fill(grid, x, y)
        let mut min_x = width
        let mut min_y = height
        let mut max_x = 0
        let mut max_y = 0
        for p in comp {
          visited[p.y * width + p.x] = true
          if p.x < min_x {
            min_x = p.x
          }
          if p.y < min_y {
            min_y = p.y
          }
          if p.x > max_x {
            max_x = p.x
          }
          if p.y > max_y {
            max_y = p.y
          }
        }
        if comp.length() >= 4 {
          rooms.push({
            rect: {
              x: min_x,
              y: min_y,
              w: max_x - min_x + 1,
              h: max_y - min_y + 1,
            },
            id: room_id,
          })
          room_id += 1
        }
      }
    }
  }
  rooms
}

///|
fn place_start_goal(grid : @types.Grid2D, width : Int, height : Int) -> Unit {
  let cx = width / 2
  let cy = height / 2
  let start = find_nearest_floor(grid, cx, cy, width, height)
  grid.set(start.x, start.y, @types.Start)
  let filled = @types.flood_fill(grid, start.x, start.y)
  let mut max_dist = 0
  let mut goal = start
  for p in filled {
    let dist = abs_int(p.x - start.x) + abs_int(p.y - start.y)
    if dist > max_dist {
      max_dist = dist
      goal = p
    }
  }
  grid.set(goal.x, goal.y, @types.Goal)
}

///|
fn find_nearest_floor(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int,
  width : Int,
  height : Int
) -> @types.Point {
  for radius in 0..<(width + height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        if abs_int(dx) + abs_int(dy) != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && grid.get(x, y) == @types.Floor {
          return { x, y }
        }
      }
    }
  }
  { x: cx, y: cy }
}
