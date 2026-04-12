///|
pub(all) struct SpelunkyConfig {
  width : Int
  height : Int
  grid_cols : Int
  grid_rows : Int
  room_min_size : Int
  room_max_size : Int
  off_path_chance : Double
}

///|
pub fn SpelunkyConfig::default() -> SpelunkyConfig {
  {
    width: 40,
    height: 20,
    grid_cols: 4,
    grid_rows: 4,
    room_min_size: 3,
    room_max_size: 5,
    off_path_chance: 0.3,
  }
}

///|
pub(all) struct SpelunkyResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  path : Array[@types.Point]
}

///|
pub fn generate(config : SpelunkyConfig, rng : @types.RNG) -> SpelunkyResult {
  let cell_w = (config.width - 2) / config.grid_cols
  let cell_h = (config.height - 2) / config.grid_rows
  // Generate random walk path from top row to bottom row
  let path = generate_path(config.grid_cols, config.grid_rows, rng)
  // Determine active cells: path cells + random off-path cells
  let active = Array::make(config.grid_cols * config.grid_rows, false)
  for p in path {
    active[p.y * config.grid_cols + p.x] = true
  }
  for row in 0..<config.grid_rows {
    for col in 0..<config.grid_cols {
      let idx = row * config.grid_cols + col
      if not(active[idx]) && rng.rand_bool(config.off_path_chance) {
        active[idx] = true
      }
    }
  }
  // Place rooms in active cells
  let rooms : Array[@types.Room] = []
  let room_grid : Array[@types.Room?] = Array::make(
    config.grid_cols * config.grid_rows,
    None,
  )
  let mut room_id = 0
  for row in 0..<config.grid_rows {
    for col in 0..<config.grid_cols {
      let idx = row * config.grid_cols + col
      if active[idx] {
        let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        // Center room in cell
        let cell_x = 1 + col * cell_w
        let cell_y = 1 + row * cell_h
        let rx = cell_x + (cell_w - rw) / 2
        let ry = cell_y + (cell_h - rh) / 2
        let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
        let room : @types.Room = { rect, id: room_id }
        rooms.push(room)
        room_grid[idx] = Some(room)
        room_id += 1
      }
    }
  }
  // Build grid
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve rooms
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect path cells with L-corridors
  for i in 1..<path.length() {
    let prev = path[i - 1]
    let curr = path[i]
    let prev_idx = prev.y * config.grid_cols + prev.x
    let curr_idx = curr.y * config.grid_cols + curr.x
    match (room_grid[prev_idx], room_grid[curr_idx]) {
      (Some(r1), Some(r2)) =>
        grid.carve_l_corridor(
          r1.rect.center(),
          r2.rect.center(),
          @types.Corridor,
        )
      _ => ()
    }
  }
  // Connect adjacent active cells that are not on path
  for row in 0..<config.grid_rows {
    for col in 0..<config.grid_cols {
      let idx = row * config.grid_cols + col
      if room_grid[idx] is None {
        continue
      }
      // Check right neighbor
      if col + 1 < config.grid_cols {
        let right_idx = row * config.grid_cols + (col + 1)
        match (room_grid[idx], room_grid[right_idx]) {
          (Some(r1), Some(r2)) => {
            // Only connect if not already on path consecutively
            if not(is_path_consecutive(path, col, row, col + 1, row)) {
              grid.carve_l_corridor(
                r1.rect.center(),
                r2.rect.center(),
                @types.Corridor,
              )
            }
          }
          _ => ()
        }
      }
      // Check below neighbor
      if row + 1 < config.grid_rows {
        let below_idx = (row + 1) * config.grid_cols + col
        match (room_grid[idx], room_grid[below_idx]) {
          (Some(r1), Some(r2)) => {
            if not(is_path_consecutive(path, col, row, col, row + 1)) {
              grid.carve_l_corridor(
                r1.rect.center(),
                r2.rect.center(),
                @types.Corridor,
              )
            }
          }
          _ => ()
        }
      }
    }
  }
  // Place doors at room perimeters where corridors touch
  for room in rooms {
    let rect = room.rect
    for x in rect.x..<(rect.x + rect.w) {
      if rect.y > 0 && grid.get(x, rect.y - 1) == @types.Corridor {
        grid.set(x, rect.y, @types.Door)
      }
      let by = rect.y + rect.h - 1
      if by + 1 < config.height && grid.get(x, by + 1) == @types.Corridor {
        grid.set(x, by, @types.Door)
      }
    }
    for y in rect.y..<(rect.y + rect.h) {
      if rect.x > 0 && grid.get(rect.x - 1, y) == @types.Corridor {
        grid.set(rect.x, y, @types.Door)
      }
      let rx = rect.x + rect.w - 1
      if rx + 1 < config.width && grid.get(rx + 1, y) == @types.Corridor {
        grid.set(rx, y, @types.Door)
      }
    }
  }
  // Place Start and Goal
  let start_cell = path[0]
  let start_idx = start_cell.y * config.grid_cols + start_cell.x
  match room_grid[start_idx] {
    Some(r) => {
      let c = r.rect.center()
      grid.set(c.x, c.y, @types.Start)
    }
    _ => ()
  }
  let goal_cell = path[path.length() - 1]
  let goal_idx = goal_cell.y * config.grid_cols + goal_cell.x
  match room_grid[goal_idx] {
    Some(r) => {
      let c = r.rect.center()
      grid.set(c.x, c.y, @types.Goal)
    }
    _ => ()
  }
  // Collect path as world-space points
  let world_path : Array[@types.Point] = []
  for p in path {
    let idx = p.y * config.grid_cols + p.x
    match room_grid[idx] {
      Some(r) => world_path.push(r.rect.center())
      _ => ()
    }
  }
  { grid, rooms, path: world_path }
}

///|
fn generate_path(
  cols : Int,
  rows : Int,
  rng : @types.RNG
) -> Array[@types.Point] {
  let path : Array[@types.Point] = []
  let mut col = rng.next_int(cols)
  let mut row = 0
  path.push({ x: col, y: row })
  let mut h_streak = 0
  while row < rows - 1 {
    // Choose direction: 0=left, 1=right, 2=down
    let dir = rng.next_int(3)
    if dir == 0 && col > 0 && h_streak < 2 {
      col -= 1
      h_streak += 1
      path.push({ x: col, y: row })
    } else if dir == 1 && col < cols - 1 && h_streak < 2 {
      col += 1
      h_streak += 1
      path.push({ x: col, y: row })
    } else {
      row += 1
      h_streak = 0
      path.push({ x: col, y: row })
    }
  }
  path
}

///|
fn is_path_consecutive(
  path : Array[@types.Point],
  x1 : Int,
  y1 : Int,
  x2 : Int,
  y2 : Int
) -> Bool {
  for i in 1..<path.length() {
    let prev = path[i - 1]
    let curr = path[i]
    if (prev.x == x1 && prev.y == y1 && curr.x == x2 && curr.y == y2) ||
      (prev.x == x2 && prev.y == y2 && curr.x == x1 && curr.y == y1) {
      return true
    }
  }
  false
}
