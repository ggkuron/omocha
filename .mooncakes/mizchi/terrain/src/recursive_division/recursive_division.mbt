///|
pub(all) struct RecursiveDivisionConfig {
  width : Int
  height : Int
  min_room_size : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn RecursiveDivisionConfig::default() -> RecursiveDivisionConfig {
  {
    width: 41,
    height: 21,
    min_room_size: 3,
    room_chance: 0.1,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct RecursiveDivisionResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : RecursiveDivisionConfig,
  rng : @types.RNG
) -> RecursiveDivisionResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Corridor)
  // Draw outer walls
  for x in 0..<w {
    grid.set(x, 0, @types.Wall)
    grid.set(x, h - 1, @types.Wall)
  }
  for y in 0..<h {
    grid.set(0, y, @types.Wall)
    grid.set(w - 1, y, @types.Wall)
  }
  // Use iterative stack-based division
  let stack : Array[(Int, Int, Int, Int)] = [(1, 1, w - 2, h - 2)]
  while stack.length() > 0 {
    let area = stack.pop().unwrap()
    let x1 = area.0
    let y1 = area.1
    let x2 = area.2
    let y2 = area.3
    let area_w = x2 - x1 + 1
    let area_h = y2 - y1 + 1
    if area_w < config.min_room_size * 2 + 1 &&
      area_h < config.min_room_size * 2 + 1 {
      continue
    }
    if area_w > area_h {
      // Vertical wall
      divide_vertical(grid, x1, y1, x2, y2, config, rng, stack)
    } else if area_h > area_w {
      // Horizontal wall
      divide_horizontal(grid, x1, y1, x2, y2, config, rng, stack)
    } else if rng.rand_bool(0.5) {
      divide_vertical(grid, x1, y1, x2, y2, config, rng, stack)
    } else {
      divide_horizontal(grid, x1, y1, x2, y2, config, rng, stack)
    }
  }
  // Place rooms
  let rooms = place_rooms(grid, w, h, config, rng)
  // Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}

///|
fn divide_vertical(
  grid : @types.Grid2D,
  x1 : Int,
  y1 : Int,
  x2 : Int,
  y2 : Int,
  config : RecursiveDivisionConfig,
  rng : @types.RNG,
  stack : Array[(Int, Int, Int, Int)]
) -> Unit {
  let area_w = x2 - x1 + 1
  if area_w < config.min_room_size * 2 + 1 {
    return
  }
  // Pick wall position on even coordinate (so passages on odd)
  let min_wx = x1 + config.min_room_size
  let max_wx = x2 - config.min_room_size
  if min_wx > max_wx {
    return
  }
  let mut wx = rng.rand_range(min_wx, max_wx + 1)
  // Ensure wx is even relative to grid
  if wx % 2 == 1 {
    wx = if wx + 1 <= max_wx { wx + 1 } else { wx - 1 }
  }
  if wx < min_wx || wx > max_wx {
    return
  }
  // Draw wall
  for y in y1..=y2 {
    grid.set(wx, y, @types.Wall)
  }
  // Carve passage
  let mut py = rng.rand_range(y1, y2 + 1)
  if py % 2 == 0 {
    py = if py + 1 <= y2 { py + 1 } else { py - 1 }
  }
  grid.set(wx, py, @types.Door)
  // Recurse on two halves
  if wx - 1 >= x1 {
    stack.push((x1, y1, wx - 1, y2))
  }
  if wx + 1 <= x2 {
    stack.push((wx + 1, y1, x2, y2))
  }
}

///|
fn divide_horizontal(
  grid : @types.Grid2D,
  x1 : Int,
  y1 : Int,
  x2 : Int,
  y2 : Int,
  config : RecursiveDivisionConfig,
  rng : @types.RNG,
  stack : Array[(Int, Int, Int, Int)]
) -> Unit {
  let area_h = y2 - y1 + 1
  if area_h < config.min_room_size * 2 + 1 {
    return
  }
  let min_wy = y1 + config.min_room_size
  let max_wy = y2 - config.min_room_size
  if min_wy > max_wy {
    return
  }
  let mut wy = rng.rand_range(min_wy, max_wy + 1)
  if wy % 2 == 1 {
    wy = if wy + 1 <= max_wy { wy + 1 } else { wy - 1 }
  }
  if wy < min_wy || wy > max_wy {
    return
  }
  // Draw wall
  for x in x1..=x2 {
    grid.set(x, wy, @types.Wall)
  }
  // Carve passage
  let mut px = rng.rand_range(x1, x2 + 1)
  if px % 2 == 0 {
    px = if px + 1 <= x2 { px + 1 } else { px - 1 }
  }
  grid.set(px, wy, @types.Door)
  // Recurse on two halves
  if wy - 1 >= y1 {
    stack.push((x1, y1, x2, wy - 1))
  }
  if wy + 1 <= y2 {
    stack.push((x1, wy + 1, x2, y2))
  }
}

///|
fn place_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  config : RecursiveDivisionConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  let attempts = w * h / 10
  for _ in 0..<attempts {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(1, w - rw - 1)
    let ry = rng.rand_range(1, h - rh - 1)
    if rx + rw < w && ry + rh < h {
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut ok = true
      for r in rooms {
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          ok = false
          break
        }
      }
      if ok {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  rooms
}
