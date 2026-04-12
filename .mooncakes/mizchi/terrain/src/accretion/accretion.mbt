///|
pub(all) struct AccretionConfig {
  width : Int
  height : Int
  max_rooms : Int
  room_min_size : Int
  room_max_size : Int
  max_attempts_per_room : Int
  first_room_size : Int
}

///|
pub fn AccretionConfig::default() -> AccretionConfig {
  {
    width: 50,
    height: 25,
    max_rooms: 12,
    room_min_size: 3,
    room_max_size: 8,
    max_attempts_per_room: 100,
    first_room_size: 6,
  }
}

///|
pub(all) struct AccretionResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : AccretionConfig, rng : @types.RNG) -> AccretionResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Place first room at center
  let fw = config.first_room_size
  let fh = config.first_room_size
  let first_rect : @types.Rect = {
    x: config.width / 2 - fw / 2,
    y: config.height / 2 - fh / 2,
    w: fw,
    h: fh,
  }
  grid.fill_rect(first_rect, @types.Floor)
  rooms.push({ rect: first_rect, id: 0 })
  // Accrete rooms one by one
  for room_id in 1..<config.max_rooms {
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let mut placed = false
    for _ in 0..<config.max_attempts_per_room {
      // Pick a random wall cell adjacent to floor
      let wall = find_random_attachment_wall(grid, config, rng)
      match wall {
        None => continue
        Some((wx, wy, dx, dy)) => {
          // Try to place room extending in direction (dx, dy) from wall
          let rx = if dx > 0 {
            wx + 1
          } else if dx < 0 {
            wx - rw
          } else {
            wx - rw / 2
          }
          let ry = if dy > 0 {
            wy + 1
          } else if dy < 0 {
            wy - rh
          } else {
            wy - rh / 2
          }
          let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
          // Check bounds
          if rect.x < 1 ||
            rect.y < 1 ||
            rect.x + rect.w >= config.width - 1 ||
            rect.y + rect.h >= config.height - 1 {
            continue
          }
          // Check if area is all wall (no overlap with existing rooms)
          if not(area_is_all_wall(grid, rect)) {
            continue
          }
          // Place the room
          grid.fill_rect(rect, @types.Floor)
          // Place door at wall cell
          grid.set(wx, wy, @types.Door)
          rooms.push({ rect, id: room_id })
          placed = true
          break
        }
      }
    }
    if not(placed) {
      break
    }
  }
  // Place Start and Goal
  if rooms.length() > 0 {
    let sc = rooms[0].rect.center()
    grid.set(sc.x, sc.y, @types.Start)
  }
  if rooms.length() > 1 {
    let gc = rooms[rooms.length() - 1].rect.center()
    grid.set(gc.x, gc.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn find_random_attachment_wall(
  grid : @types.Grid2D,
  config : AccretionConfig,
  rng : @types.RNG
) -> (Int, Int, Int, Int)? {
  // Try random positions to find wall adjacent to floor
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  for _ in 0..<50 {
    let x = rng.rand_range(1, config.width - 1)
    let y = rng.rand_range(1, config.height - 1)
    if grid.get(x, y) != @types.Wall {
      continue
    }
    // Check if any neighbor is floor
    for d in dirs {
      let nx = x + d.0
      let ny = y + d.1
      if grid.in_bounds(nx, ny) {
        let t = grid.get(nx, ny)
        if t == @types.Floor || t == @types.Start || t == @types.Goal {
          // Wall faces inward (toward floor), room extends outward
          return Some((x, y, -d.0, -d.1))
        }
      }
    }
  }
  None
}

///|
fn area_is_all_wall(grid : @types.Grid2D, rect : @types.Rect) -> Bool {
  // Check the rect plus 1-cell padding is all wall
  for y in (rect.y - 1)..<(rect.y + rect.h + 1) {
    for x in (rect.x - 1)..<(rect.x + rect.w + 1) {
      if not(grid.in_bounds(x, y)) {
        return false
      }
      if grid.get(x, y) != @types.Wall {
        return false
      }
    }
  }
  true
}
