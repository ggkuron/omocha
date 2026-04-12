///|
pub(all) struct HilbertConfig {
  width : Int
  height : Int
  order : Int
  corridor_width : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn HilbertConfig::default() -> HilbertConfig {
  {
    width: 50,
    height: 25,
    order: 4,
    corridor_width: 1,
    room_chance: 0.08,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct HilbertResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : HilbertConfig, rng : @types.RNG) -> HilbertResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Generate Hilbert curve points
  let n = 1 << config.order // 2^order
  let total = n * n
  let points : Array[@types.Point] = []
  for i in 0..<total {
    let p = d2xy(n, i)
    // Scale to grid
    let margin_x = 2
    let margin_y = 2
    let usable_w = config.width - margin_x * 2
    let usable_h = config.height - margin_y * 2
    let gx = margin_x + p.x * usable_w / (n - 1).max(1)
    let gy = margin_y + p.y * usable_h / (n - 1).max(1)
    if gx > 0 && gx < config.width - 1 && gy > 0 && gy < config.height - 1 {
      points.push({ x: gx, y: gy })
    }
  }
  // Carve corridors along Hilbert path
  for i in 0..<points.length() {
    grid.set(points[i].x, points[i].y, @types.Corridor)
    if i > 0 {
      carve_line_wide(grid, points[i - 1], points[i], config.corridor_width)
    }
  }
  // Place rooms
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for p in points {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = p.x - rw / 2
    let ry = p.y - rh / 2
    if rx > 0 && ry > 0 && rx + rw < config.width - 1 && ry + rh < config.height -
    1 {
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
  // Place Start and Goal at curve endpoints
  if points.length() > 0 {
    grid.set(points[0].x, points[0].y, @types.Start)
    grid.set(points[points.length() - 1].x, points[points.length() - 1].y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn d2xy(n : Int, d : Int) -> @types.Point {
  let mut rx = 0
  let mut ry = 0
  let mut x = 0
  let mut y = 0
  let mut dd = d
  let mut s = 1
  while s < n {
    rx = if (dd & 2) != 0 { 1 } else { 0 }
    ry = if (dd & 1) != 0 {
      if rx == 0 { 1 } else { 0 }
    } else {
      0
    }
    // Rotate
    if ry == 0 {
      if rx == 1 {
        x = s - 1 - x
        y = s - 1 - y
      }
      let tmp = x
      x = y
      y = tmp
    }
    x = x + s * rx
    y = y + s * ry
    dd = dd / 4
    s = s * 2
  }
  { x, y }
}

///|
fn carve_line_wide(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  _width : Int
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if grid.in_bounds(x, y) {
      grid.set(x, y, @types.Corridor)
    }
    if x != to.x {
      x = if x < to.x { x + 1 } else { x - 1 }
    } else if y != to.y {
      y = if y < to.y { y + 1 } else { y - 1 }
    }
  }
  if grid.in_bounds(to.x, to.y) {
    grid.set(to.x, to.y, @types.Corridor)
  }
}
