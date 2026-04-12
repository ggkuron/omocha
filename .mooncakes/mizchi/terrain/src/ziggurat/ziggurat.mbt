///|
pub(all) struct ZigguratConfig {
  width : Int
  height : Int
  max_height : Int
  num_levels : Int
  room_chance : Double
}

///|
pub fn ZigguratConfig::default() -> ZigguratConfig {
  { width: 48, height: 48, max_height: 8, num_levels: 4, room_chance: 0.3 }
}

///|
pub(all) struct ZigguratResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : ZigguratConfig, rng : @types.RNG) -> ZigguratResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Initialize all cells to height 0
  for y in 0..<h {
    for x in 0..<w {
      grid.set_height(x, y, 0)
    }
  }
  // Build concentric rectangular layers for the ziggurat
  // Each layer is smaller and higher than the one below
  let num_levels = config.num_levels.max(1)
  // Calculate padding per level so they fit concentrically
  let pad_x = (w - 4) / (num_levels * 2)
  let pad_y = (h - 4) / (num_levels * 2)
  let height_step = config.max_height / num_levels
  // Store layer rects for later room carving
  let layer_rects : Array[@types.Rect] = []
  for level in 0..<num_levels {
    let inset = level * pad_x.max(2)
    let inset_y = level * pad_y.max(2)
    let lx = inset
    let ly = inset_y
    let lw = w - inset * 2
    let lh = h - inset_y * 2
    if lw < 4 || lh < 4 {
      break
    }
    let layer_height = (level + 1) * height_step
    let clamped_height = layer_height.min(config.max_height)
    let rect : @types.Rect = { x: lx, y: ly, w: lw, h: lh }
    layer_rects.push(rect)
    // Fill this layer: floor with the layer's height
    for dy in 0..<lh {
      for dx in 0..<lw {
        let px = lx + dx
        let py = ly + dy
        if grid.in_bounds(px, py) {
          grid.set(px, py, @types.Floor)
          grid.set_height(px, py, clamped_height)
        }
      }
    }
    // Create walls at the border of this layer (step edges)
    // Top and bottom edges
    for dx in 0..<lw {
      let px = lx + dx
      if grid.in_bounds(px, ly) {
        grid.set(px, ly, @types.Wall)
        grid.set_height(px, ly, clamped_height)
      }
      let bot_y = ly + lh - 1
      if grid.in_bounds(px, bot_y) {
        grid.set(px, bot_y, @types.Wall)
        grid.set_height(px, bot_y, clamped_height)
      }
    }
    // Left and right edges
    for dy in 0..<lh {
      let py = ly + dy
      if grid.in_bounds(lx, py) {
        grid.set(lx, py, @types.Wall)
        grid.set_height(lx, py, clamped_height)
      }
      let right_x = lx + lw - 1
      if grid.in_bounds(right_x, py) {
        grid.set(right_x, py, @types.Wall)
        grid.set_height(right_x, py, clamped_height)
      }
    }
  }
  // Place stairs (Door tiles) connecting levels
  // For each level boundary, place a door on each side (N, S, E, W)
  for level in 0..<(layer_rects.length() - 1) {
    let outer = layer_rects[level]
    let inner = layer_rects[level + 1]
    // North side stair: midpoint of inner top wall
    let north_x = inner.x + inner.w / 2
    let north_y = inner.y
    place_stair(grid, north_x, north_y)
    // South side stair
    let south_x = inner.x + inner.w / 2
    let south_y = inner.y + inner.h - 1
    place_stair(grid, south_x, south_y)
    // East side stair
    let east_x = inner.x + inner.w - 1
    let east_y = inner.y + inner.h / 2
    place_stair(grid, east_x, east_y)
    // West side stair
    let west_x = inner.x
    let west_y = inner.y + inner.h / 2
    place_stair(grid, west_x, west_y)
    // Also ensure adjacent cells on the outer side are floor for access
    let _outer_h = (level + 1) * height_step
    ensure_stair_access(grid, north_x, north_y - 1)
    ensure_stair_access(grid, south_x, south_y + 1)
    ensure_stair_access(grid, east_x + 1, east_y)
    ensure_stair_access(grid, west_x - 1, west_y)
    ignore(outer)
  }
  // Carve rooms inside each level
  for level in 0..<layer_rects.length() {
    let rect = layer_rects[level]
    let layer_height = ((level + 1) * height_step).min(config.max_height)
    // Interior area (inside the walls)
    let interior_x = rect.x + 2
    let interior_y = rect.y + 2
    let interior_w = rect.w - 4
    let interior_h = rect.h - 4
    if interior_w < 5 || interior_h < 5 {
      continue
    }
    // Try to place several rooms on this level
    let room_attempts = rng.rand_range(2, 5)
    for _ in 0..<room_attempts {
      if not(rng.rand_bool(config.room_chance)) {
        continue
      }
      let rw = rng.rand_range(3, (interior_w / 2).max(4))
      let rh = rng.rand_range(3, (interior_h / 2).max(4))
      let rx = rng.rand_range(interior_x, interior_x + interior_w - rw)
      let ry = rng.rand_range(interior_y, interior_y + interior_h - rh)
      if rx + rw >= rect.x + rect.w - 1 || ry + rh >= rect.y + rect.h - 1 {
        continue
      }
      // Check no overlap with existing rooms
      let room_rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for existing in rooms {
        let expanded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if room_rect.intersects(expanded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        // Carve the room with walls around it
        for dy in 0..<rh {
          for dx in 0..<rw {
            let px = rx + dx
            let py = ry + dy
            if grid.in_bounds(px, py) {
              if dx == 0 || dx == rw - 1 || dy == 0 || dy == rh - 1 {
                grid.set(px, py, @types.Wall)
              } else {
                grid.set(px, py, @types.Floor)
              }
              grid.set_height(px, py, layer_height)
            }
          }
        }
        // Place a door on a random side
        let side = rng.next_int(4)
        if side == 0 && rh > 2 {
          // North
          let dx = rng.rand_range(1, rw - 1)
          place_stair(grid, rx + dx, ry)
        } else if side == 1 && rh > 2 {
          // South
          let dx = rng.rand_range(1, rw - 1)
          place_stair(grid, rx + dx, ry + rh - 1)
        } else if side == 2 && rw > 2 {
          // West
          let dy = rng.rand_range(1, rh - 1)
          place_stair(grid, rx, ry + dy)
        } else if rw > 2 {
          // East
          let dy = rng.rand_range(1, rh - 1)
          place_stair(grid, rx + rw - 1, ry + dy)
        }
        rooms.push({ rect: room_rect, id: room_id })
        room_id += 1
      }
    }
  }
  // Create the central chamber at the topmost level
  if layer_rects.length() > 0 {
    let top_rect = layer_rects[layer_rects.length() - 1]
    let top_height = config.max_height
    let cx = top_rect.x + top_rect.w / 2
    let cy = top_rect.y + top_rect.h / 2
    let chamber_size = 3
    let chamber_rect : @types.Rect = {
      x: cx - chamber_size / 2,
      y: cy - chamber_size / 2,
      w: chamber_size,
      h: chamber_size,
    }
    for dy in 0..<chamber_size {
      for dx in 0..<chamber_size {
        let px = cx - chamber_size / 2 + dx
        let py = cy - chamber_size / 2 + dy
        if grid.in_bounds(px, py) {
          grid.set(px, py, @types.Floor)
          grid.set_height(px, py, top_height)
        }
      }
    }
    rooms.push({ rect: chamber_rect, id: room_id })
    room_id += 1
    // Place Goal at the center of the top chamber
    grid.set(cx, cy, @types.Goal)
  }
  // Place Start at the ground level (outermost layer edge)
  let start_x = 1
  let start_y = h / 2
  if grid.in_bounds(start_x, start_y) {
    grid.set(start_x, start_y, @types.Start)
    grid.set_height(start_x, start_y, height_step.min(config.max_height))
  }
  ignore(room_id)
  { grid, rooms }
}

///|
fn place_stair(grid : @types.HeightGrid, x : Int, y : Int) -> Unit {
  if grid.in_bounds(x, y) {
    grid.set(x, y, @types.Door)
  }
}

///|
fn ensure_stair_access(grid : @types.HeightGrid, x : Int, y : Int) -> Unit {
  if grid.in_bounds(x, y) && grid.get(x, y) == @types.Wall {
    grid.set(x, y, @types.Floor)
  }
}
