///|
pub(all) struct GravityCollapseConfig {
  width : Int
  height : Int
  num_rooms : Int
  room_min_size : Int
  room_max_size : Int
  corridor_width : Int
  gap : Int
}

///|
pub fn GravityCollapseConfig::default() -> GravityCollapseConfig {
  {
    width: 50,
    height: 25,
    num_rooms: 12,
    room_min_size: 3,
    room_max_size: 7,
    corridor_width: 1,
    gap: 0,
  }
}

///|
pub(all) struct GravityCollapseResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : GravityCollapseConfig,
  rng : @types.RNG
) -> GravityCollapseResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Collision map: true if occupied by a room
  let occupied : Array[Bool] = Array::make(w * h, false)
  // Mark the floor (bottom row) as occupied so rooms land on it
  for x in 0..<w {
    occupied[(h - 1) * w + x] = true
  }
  for room_id in 0..<config.num_rooms {
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    // Random x position (leave 1-tile border)
    let rx = rng.rand_range(1, w - rw - 1)
    if rx < 1 || rx + rw >= w - 1 {
      continue
    }
    // Drop from top: find the lowest y where the room can rest
    let mut land_y = -1
    for drop_y in 1..<(h - rh) {
      // Check if the row just below the room is occupied
      let mut blocked = false
      let check_y = drop_y + rh + config.gap
      if check_y >= h {
        // Hit the bottom boundary
        land_y = drop_y
        blocked = true
      } else {
        for cx in rx..<(rx + rw) {
          if occupied[check_y * w + cx] {
            blocked = true
            break
          }
        }
      }
      if blocked {
        land_y = drop_y
        break
      }
    }
    if land_y < 1 {
      continue
    }
    // Check that the room area is free
    let mut area_clear = true
    for cy in land_y..<(land_y + rh) {
      for cx in rx..<(rx + rw) {
        if occupied[cy * w + cx] {
          area_clear = false
          break
        }
      }
      if not(area_clear) {
        break
      }
    }
    if not(area_clear) {
      continue
    }
    // Place the room
    let rect : @types.Rect = { x: rx, y: land_y, w: rw, h: rh }
    grid.fill_rect(rect, @types.Floor)
    // Mark occupied
    for cy in land_y..<(land_y + rh) {
      for cx in rx..<(rx + rw) {
        occupied[cy * w + cx] = true
      }
    }
    rooms.push({ rect, id: room_id })
  }
  // Connect adjacent rooms with corridors
  for i in 0..<rooms.length() {
    // Find the nearest room to connect to
    let r1 = rooms[i]
    let c1 = r1.rect.center()
    let mut best_j = -1
    let mut best_dist = 999999
    for j in 0..<rooms.length() {
      if j == i {
        continue
      }
      let r2 = rooms[j]
      let c2 = r2.rect.center()
      let dx = c1.x - c2.x
      let dy = c1.y - c2.y
      let dist = dx * dx + dy * dy
      if dist < best_dist {
        best_dist = dist
        best_j = j
      }
    }
    if best_j >= 0 {
      let c2 = rooms[best_j].rect.center()
      // Carve corridor with given width
      carve_wide_corridor(grid, c1, c2, config.corridor_width)
    }
  }
  { grid, rooms }
}

///|
fn carve_wide_corridor(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  width : Int
) -> Unit {
  let half = width / 2
  // L-shaped corridor: horizontal then vertical
  let mut x = from.x
  let step_x = if to.x > from.x { 1 } else { -1 }
  while x != to.x {
    for dy in -(half)..=(half) {
      if grid.in_bounds(x, from.y + dy) {
        grid.set(x, from.y + dy, @types.Corridor)
      }
    }
    x = x + step_x
  }
  let mut y = from.y
  let step_y = if to.y > from.y { 1 } else { -1 }
  while y != to.y {
    for dx in -(half)..=(half) {
      if grid.in_bounds(to.x + dx, y) {
        grid.set(to.x + dx, y, @types.Corridor)
      }
    }
    y = y + step_y
  }
}
