///|
pub(all) struct RogueConfig {
  width : Int
  height : Int
  grid_cols : Int
  grid_rows : Int
  room_min_ratio : Double
  room_max_ratio : Double
  skip_chance : Double
}

///|
pub fn RogueConfig::default() -> RogueConfig {
  {
    width: 50,
    height: 25,
    grid_cols: 3,
    grid_rows: 3,
    room_min_ratio: 0.4,
    room_max_ratio: 0.8,
    skip_chance: 0.15,
  }
}

///|
pub(all) struct RogueResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
pub fn generate(config : RogueConfig, rng : @types.RNG) -> RogueResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let cell_w = (config.width - 1) / config.grid_cols
  let cell_h = (config.height - 1) / config.grid_rows
  let rooms : Array[@types.Room] = []
  let edges : Array[@types.Edge] = []
  // room_at[row][col] = room index or -1
  let room_at = Array::make(config.grid_rows * config.grid_cols, -1)
  // Phase 1: Place a room in each grid cell (some may be skipped)
  let mut room_id = 0
  for row in 0..<config.grid_rows {
    for col in 0..<config.grid_cols {
      if rng.rand_bool(config.skip_chance) {
        continue
      }
      let cx = 1 + col * cell_w
      let cy = 1 + row * cell_h
      let max_w = cell_w - 2
      let max_h = cell_h - 2
      let rw = rng.rand_range(
        (max_w.to_double() * config.room_min_ratio).to_int().max(3),
        (max_w.to_double() * config.room_max_ratio).to_int().max(4) + 1,
      )
      let rh = rng.rand_range(
        (max_h.to_double() * config.room_min_ratio).to_int().max(3),
        (max_h.to_double() * config.room_max_ratio).to_int().max(4) + 1,
      )
      let rx = cx + rng.rand_range(1, (cell_w - rw).max(1) + 1)
      let ry = cy + rng.rand_range(1, (cell_h - rh).max(1) + 1)
      let rect : @types.Rect = {
        x: rx.clamp(min=1, max=config.width - rw - 1),
        y: ry.clamp(min=1, max=config.height - rh - 1),
        w: rw,
        h: rh,
      }
      grid.fill_rect(rect, @types.Floor)
      rooms.push({ rect, id: room_id })
      room_at[row * config.grid_cols + col] = room_id
      room_id += 1
    }
  }
  // Phase 2: Ensure every cell with a room connects to neighbors
  // Connect horizontally adjacent rooms
  for row in 0..<config.grid_rows {
    for col in 0..<(config.grid_cols - 1) {
      let r1 = room_at[row * config.grid_cols + col]
      let r2 = room_at[row * config.grid_cols + (col + 1)]
      if r1 >= 0 && r2 >= 0 {
        let c1 = rooms[r1].rect.center()
        let c2 = rooms[r2].rect.center()
        grid.carve_l_corridor(c1, c2, @types.Corridor)
        edges.push({ from: r1, to: r2 })
      }
    }
  }
  // Connect vertically adjacent rooms
  for row in 0..<(config.grid_rows - 1) {
    for col in 0..<config.grid_cols {
      let r1 = room_at[row * config.grid_cols + col]
      let r2 = room_at[(row + 1) * config.grid_cols + col]
      if r1 >= 0 && r2 >= 0 {
        let c1 = rooms[r1].rect.center()
        let c2 = rooms[r2].rect.center()
        grid.carve_l_corridor(c1, c2, @types.Corridor)
        edges.push({ from: r1, to: r2 })
      }
    }
  }
  // Phase 3: Ensure full connectivity via union-find
  if rooms.length() > 1 {
    let parent = Array::makei(rooms.length(), fn(i) { i })
    for e in edges {
      union(parent, e.from, e.to)
    }
    // Find disconnected components and connect them
    for i in 1..<rooms.length() {
      if find_root(parent, i) != find_root(parent, 0) {
        let c1 = rooms[i].rect.center()
        let c2 = rooms[0].rect.center()
        grid.carve_l_corridor(c1, c2, @types.Corridor)
        edges.push({ from: i, to: 0 })
        union(parent, i, 0)
      }
    }
  }
  // Place doors at room perimeters
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
  // Place Start in first room, Goal in last room
  let sc = rooms[0].rect.center()
  grid.set(sc.x, sc.y, @types.Start)
  let gc = rooms[rooms.length() - 1].rect.center()
  grid.set(gc.x, gc.y, @types.Goal)
  { grid, rooms, edges }
}

///|
fn find_root(parent : Array[Int], x : Int) -> Int {
  let mut current = x
  while parent[current] != current {
    parent[current] = parent[parent[current]]
    current = parent[current]
  }
  current
}

///|
fn union(parent : Array[Int], a : Int, b : Int) -> Unit {
  let ra = find_root(parent, a)
  let rb = find_root(parent, b)
  if ra != rb {
    parent[ra] = rb
  }
}
