///|
pub(all) struct SwampConfig {
  width : Int
  height : Int
  water_ratio : Double
  path_count : Int
}

///|
pub fn SwampConfig::default() -> SwampConfig {
  { width: 50, height: 30, water_ratio: 0.4, path_count: 3 }
}

///|
pub(all) struct SwampResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : SwampConfig, rng : @types.RNG) -> SwampResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Floor)
  let rooms : Array[@types.Room] = []
  // Step 1: Generate water channels using noise-like approach
  // Seed random water cells
  for y in 0..<h {
    for x in 0..<w {
      if rng.rand_bool(config.water_ratio * 0.7) {
        grid.set(x, y, @types.Water)
      }
    }
  }
  // Cellular automata to create organic shorelines
  for _ in 0..<4 {
    let old = grid.clone()
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let water_count = count_tile(old, x, y, @types.Water)
        if old.get(x, y) == @types.Water {
          // Water stays if enough neighbors are water
          if water_count < 3 {
            grid.set(x, y, @types.Floor)
          }
        } else if water_count >= 5 {
          grid.set(x, y, @types.Water)
        }
      }
    }
  }
  // Step 2: Add sparse trees on land
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if grid.get(x, y) == @types.Floor && rng.rand_bool(0.12) {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Step 3: Place islands with small rooms
  let mut room_id = 0
  let num_islands = rng.rand_range(3, 6)
  for _ in 0..<num_islands {
    let rw = rng.rand_range(3, 6)
    let rh = rng.rand_range(3, 5)
    let rx = rng.rand_range(2, w - rw - 2)
    let ry = rng.rand_range(2, h - rh - 2)
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    // Clear the island area
    for cy in ry..<(ry + rh) {
      for cx in rx..<(rx + rw) {
        if grid.in_bounds(cx, cy) {
          grid.set(cx, cy, @types.Floor)
        }
      }
    }
    // Add a wall border around the island
    for cx in rx..<(rx + rw) {
      if grid.in_bounds(cx, ry) {
        grid.set(cx, ry, @types.Wall)
      }
      if grid.in_bounds(cx, ry + rh - 1) {
        grid.set(cx, ry + rh - 1, @types.Wall)
      }
    }
    for cy in ry..<(ry + rh) {
      if grid.in_bounds(rx, cy) {
        grid.set(rx, cy, @types.Wall)
      }
      if grid.in_bounds(rx + rw - 1, cy) {
        grid.set(rx + rw - 1, cy, @types.Wall)
      }
    }
    // Door entrance
    let door_side = rng.next_int(4)
    match door_side {
      0 => {
        let dx = rx + rw / 2
        if grid.in_bounds(dx, ry) {
          grid.set(dx, ry, @types.Door)
        }
      }
      1 => {
        let dx = rx + rw / 2
        if grid.in_bounds(dx, ry + rh - 1) {
          grid.set(dx, ry + rh - 1, @types.Door)
        }
      }
      2 => {
        let dy = ry + rh / 2
        if grid.in_bounds(rx, dy) {
          grid.set(rx, dy, @types.Door)
        }
      }
      _ => {
        let dy = ry + rh / 2
        if grid.in_bounds(rx + rw - 1, dy) {
          grid.set(rx + rw - 1, dy, @types.Door)
        }
      }
    }
    rooms.push({ rect, id: room_id })
    room_id += 1
  }
  // Step 4: Create stepping stone paths across water
  for _ in 0..<config.path_count {
    // Pick random start and end on opposite sides
    let start_x = rng.rand_range(1, w / 4)
    let start_y = rng.rand_range(1, h - 1)
    let end_x = rng.rand_range(w * 3 / 4, w - 1)
    let end_y = rng.rand_range(1, h - 1)
    // Carve stepping stones
    let mut x = start_x
    let mut y = start_y
    while x != end_x || y != end_y {
      if grid.in_bounds(x, y) && grid.get(x, y) == @types.Water {
        grid.set(x, y, @types.Corridor)
      }
      // Move toward target with some randomness
      if rng.rand_bool(0.2) {
        // Random step
        let dir = rng.rand_direction()
        let nx = x + dir.x
        let ny = y + dir.y
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
          x = nx
          y = ny
        }
      } else if x != end_x && (y == end_y || rng.rand_bool(0.5)) {
        if x < end_x {
          x += 1
        } else {
          x -= 1
        }
      } else if y != end_y {
        if y < end_y {
          y += 1
        } else {
          y -= 1
        }
      }
    }
  }
  // Place Start and Goal
  let start_pos = find_walkable(grid, 2, h / 2)
  grid.set(start_pos.x, start_pos.y, @types.Start)
  let goal_pos = find_walkable(grid, w - 3, h / 2)
  if goal_pos.x != start_pos.x || goal_pos.y != start_pos.y {
    grid.set(goal_pos.x, goal_pos.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn count_tile(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  tile : @types.Tile
) -> Int {
  let mut count = 0
  for dy in -1..=1 {
    for dx in -1..=1 {
      if dx == 0 && dy == 0 {
        continue
      }
      let nx = x + dx
      let ny = y + dy
      if grid.in_bounds(nx, ny) && grid.get(nx, ny) == tile {
        count += 1
      }
    }
  }
  count
}

///|
fn find_walkable(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let adx = if dx < 0 { -dx } else { dx }
        let ady = if dy < 0 { -dy } else { dy }
        if adx + ady != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) {
          let t = grid.get(x, y)
          if t == @types.Floor || t == @types.Corridor {
            return { x, y }
          }
        }
      }
    }
  }
  { x: cx, y: cy }
}
