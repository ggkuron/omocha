///|
pub(all) struct GosperConfig {
  width : Int
  height : Int
  order : Int
  corridor_width : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn GosperConfig::default() -> GosperConfig {
  {
    width: 50,
    height: 25,
    order: 3,
    corridor_width: 1,
    room_chance: 0.1,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct GosperResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

// Expand the Gosper curve L-system string
// Axiom: "A"
// Rules: A -> "A-B--B+A++AA+B-"
//        B -> "+A-BB--B-A++A+B"
///|
fn expand_gosper(order : Int) -> String {
  let mut current = "A"
  for _ in 0..<order {
    let next = StringBuilder::new()
    let chars = current.to_array()
    for c in chars {
      match c {
        'A' => next.write_string("A-B--B+A++AA+B-")
        'B' => next.write_string("+A-BB--B-A++A+B")
        _ => next.write_char(c)
      }
    }
    current = next.to_string()
    // Limit to prevent explosion
    if current.to_array().length() > 50000 {
      break
    }
  }
  current
}

// 6-direction movement for hex-like grid
// Direction 0..5, using offset coordinate system
// dx/dy for each of 6 directions (pointy-top hex mapped to grid):
//   0: East      (1, 0)
//   1: NE        (1, -1)
//   2: NW        (0, -1)
//   3: West      (-1, 0)
//   4: SW        (-1, 1)
//   5: SE        (0, 1)
///|
fn hex_dx(dir : Int) -> Int {
  let d = ((dir % 6) + 6) % 6
  match d {
    0 => 1
    1 => 1
    2 => 0
    3 => -1
    4 => -1
    5 => 0
    _ => 0
  }
}

///|
fn hex_dy(dir : Int) -> Int {
  let d = ((dir % 6) + 6) % 6
  match d {
    0 => 0
    1 => -1
    2 => -1
    3 => 0
    4 => 1
    5 => 1
    _ => 0
  }
}

///|
pub fn generate(config : GosperConfig, rng : @types.RNG) -> GosperResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Expand the L-system
  let lstr = expand_gosper(config.order)
  let chars = lstr.to_array()
  // First pass: compute raw path coordinates to find bounding box
  let raw_points : Array[(Int, Int)] = []
  let mut rx = 0
  let mut ry = 0
  let mut dir = 0
  let turning_indices : Array[Int] = []
  raw_points.push((rx, ry))
  for c in chars {
    match c {
      'A' | 'B' => {
        // Move forward in current direction
        rx = rx + hex_dx(dir)
        ry = ry + hex_dy(dir)
        raw_points.push((rx, ry))
      }
      '+' => {
        // Turn left 60 degrees
        dir = ((dir + 1) % 6 + 6) % 6
        turning_indices.push(raw_points.length() - 1)
      }
      '-' => {
        // Turn right 60 degrees
        dir = ((dir - 1) % 6 + 6) % 6
        turning_indices.push(raw_points.length() - 1)
      }
      _ => ()
    }
  }
  if raw_points.length() == 0 {
    return { grid, rooms }
  }
  // Find bounding box of raw points
  let mut min_x = raw_points[0].0
  let mut max_x = raw_points[0].0
  let mut min_y = raw_points[0].1
  let mut max_y = raw_points[0].1
  for p in raw_points {
    if p.0 < min_x {
      min_x = p.0
    }
    if p.0 > max_x {
      max_x = p.0
    }
    if p.1 < min_y {
      min_y = p.1
    }
    if p.1 > max_y {
      max_y = p.1
    }
  }
  let raw_w = max_x - min_x + 1
  let raw_h = max_y - min_y + 1
  // Map raw coordinates to grid coordinates with margins
  let margin = 2
  let usable_w = config.width - margin * 2
  let usable_h = config.height - margin * 2
  // Scale factors
  let scale_x = if raw_w > 1 {
    usable_w.to_double() / raw_w.to_double()
  } else {
    1.0
  }
  let scale_y = if raw_h > 1 {
    usable_h.to_double() / raw_h.to_double()
  } else {
    1.0
  }
  // Use uniform scale to preserve shape
  let scale = if scale_x < scale_y { scale_x } else { scale_y }
  // Convert and carve
  let grid_points : Array[@types.Point] = []
  let mut prev_gx = -1
  let mut prev_gy = -1
  for p in raw_points {
    let gx = margin +
      ((p.0 - min_x).to_double() * scale).to_int()
    let gy = margin +
      ((p.1 - min_y).to_double() * scale).to_int()
    // Dedup consecutive identical grid points
    if gx == prev_gx && gy == prev_gy {
      grid_points.push({ x: gx, y: gy })
      continue
    }
    prev_gx = gx
    prev_gy = gy
    grid_points.push({ x: gx, y: gy })
    // Carve this point
    carve_wide_gosper(grid, gx, gy, config.corridor_width)
    // Connect to previous
    if grid_points.length() > 1 {
      let prev = grid_points[grid_points.length() - 2]
      carve_line_gosper(grid, prev.x, prev.y, gx, gy, config.corridor_width)
    }
  }
  // Place rooms at turning points
  let mut room_id = 0
  for idx in turning_indices {
    if idx >= grid_points.length() {
      continue
    }
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let p = grid_points[idx]
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = p.x - rw / 2
    let ry = p.y - rh / 2
    if rx < 1 || ry < 1 || rx + rw >= config.width - 1 || ry + rh >= config.height -
    1 {
      continue
    }
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
  // Place start and goal
  if grid_points.length() > 0 {
    let start = grid_points[0]
    if grid.in_bounds(start.x, start.y) {
      grid.set(start.x, start.y, @types.Start)
    }
    let goal = grid_points[grid_points.length() - 1]
    if grid.in_bounds(goal.x, goal.y) {
      grid.set(goal.x, goal.y, @types.Goal)
    }
  }
  { grid, rooms }
}

///|
fn carve_wide_gosper(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  width : Int
) -> Unit {
  let half = width / 2
  for dy in -half..=half {
    for dx in -half..=half {
      let nx = x + dx
      let ny = y + dy
      if grid.in_bounds(nx, ny) && grid.get(nx, ny) == @types.Wall {
        grid.set(nx, ny, @types.Corridor)
      }
    }
  }
}

///|
fn carve_line_gosper(
  grid : @types.Grid2D,
  x0 : Int,
  y0 : Int,
  x1 : Int,
  y1 : Int,
  width : Int
) -> Unit {
  let mut x = x0
  let mut y = y0
  while x != x1 || y != y1 {
    carve_wide_gosper(grid, x, y, width)
    if x != x1 {
      x = if x < x1 { x + 1 } else { x - 1 }
    } else if y != y1 {
      y = if y < y1 { y + 1 } else { y - 1 }
    }
  }
  carve_wide_gosper(grid, x1, y1, width)
}
