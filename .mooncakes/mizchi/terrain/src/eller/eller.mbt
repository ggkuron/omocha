///|
pub(all) struct EllerConfig {
  width : Int
  height : Int
  merge_chance : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn EllerConfig::default() -> EllerConfig {
  {
    width: 41,
    height: 21,
    merge_chance: 0.5,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct EllerResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : EllerConfig, rng : @types.RNG) -> EllerResult {
  // Ensure odd dimensions for proper maze cells
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  // Cell sets: each cell belongs to a set ID
  let sets = Array::make(maze_w, 0)
  let mut next_set = 0
  // Initialize first row: each cell gets unique set
  for i in 0..<maze_w {
    sets[i] = next_set
    next_set += 1
  }
  for row in 0..<maze_h {
    let gy = row * 2 + 1
    // Carve cells in this row
    for col in 0..<maze_w {
      let gx = col * 2 + 1
      grid.set(gx, gy, @types.Corridor)
    }
    // Merge adjacent cells in same row
    if row < maze_h - 1 {
      for col in 0..<(maze_w - 1) {
        if sets[col] != sets[col + 1] && rng.rand_bool(config.merge_chance) {
          // Remove wall between col and col+1
          let gx = col * 2 + 2
          grid.set(gx, gy, @types.Corridor)
          // Merge sets
          let old_set = sets[col + 1]
          let new_set = sets[col]
          for k in 0..<maze_w {
            if sets[k] == old_set {
              sets[k] = new_set
            }
          }
        }
      }
      // Create vertical connections: at least one per set
      // Collect cells by set
      let set_cells : Array[Array[Int]] = []
      let set_ids : Array[Int] = []
      for col in 0..<maze_w {
        let sid = sets[col]
        let mut found = false
        for i in 0..<set_ids.length() {
          if set_ids[i] == sid {
            set_cells[i].push(col)
            found = true
            break
          }
        }
        if not(found) {
          set_ids.push(sid)
          set_cells.push([col])
        }
      }
      // For each set, ensure at least one vertical connection
      let has_down = Array::make(maze_w, false)
      for i in 0..<set_ids.length() {
        let cells = set_cells[i]
        // Shuffle and pick at least one
        let first = rng.next_int(cells.length())
        let col = cells[first]
        let gx = col * 2 + 1
        grid.set(gx, gy + 1, @types.Corridor) // wall between rows
        has_down[col] = true
        // Randomly add more
        for j in 0..<cells.length() {
          if j != first && rng.rand_bool(config.merge_chance) {
            let c = cells[j]
            let cgx = c * 2 + 1
            grid.set(cgx, gy + 1, @types.Corridor)
            has_down[c] = true
          }
        }
      }
      // Prepare next row
      let new_sets = Array::make(maze_w, 0)
      for col in 0..<maze_w {
        if has_down[col] {
          new_sets[col] = sets[col]
        } else {
          new_sets[col] = next_set
          next_set += 1
        }
      }
      for col in 0..<maze_w {
        sets[col] = new_sets[col]
      }
    } else {
      // Last row: merge all adjacent cells in different sets
      for col in 0..<(maze_w - 1) {
        if sets[col] != sets[col + 1] {
          let gx = col * 2 + 2
          grid.set(gx, gy, @types.Corridor)
          let old_set = sets[col + 1]
          let new_set = sets[col]
          for k in 0..<maze_w {
            if sets[k] == old_set {
              sets[k] = new_set
            }
          }
        }
      }
    }
  }
  // Place rooms at random locations (overwriting maze)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for _ in 0..<(maze_w * maze_h) {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(0, ((w - rw - 1) / 2).max(1)) * 2 + 1
    let ry = rng.rand_range(0, ((h - rh - 1) / 2).max(1)) * 2 + 1
    if rx + rw < w && ry + rh < h {
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for r in rooms {
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  // Place Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}
