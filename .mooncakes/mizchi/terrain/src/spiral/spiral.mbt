///|
pub(all) struct SpiralConfig {
  width : Int
  height : Int
  spacing : Int
  corridor_width : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
  num_turns : Int
}

///|
pub fn SpiralConfig::default() -> SpiralConfig {
  {
    width: 50,
    height: 25,
    spacing: 3,
    corridor_width: 1,
    room_chance: 0.2,
    room_min_size: 3,
    room_max_size: 5,
    num_turns: 5,
  }
}

///|
pub(all) struct SpiralResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

// Simple sin/cos approximation via Taylor series (sufficient for dungeon gen)
///|
fn approx_sin(x : Double) -> Double {
  // Normalize to [-pi, pi]
  let pi = 3.14159265358979
  let two_pi = 6.28318530717959
  let mut a = x
  // Bring into range
  while a > pi {
    a = a - two_pi
  }
  while a < -pi {
    a = a + two_pi
  }
  // Taylor series: sin(x) ~ x - x^3/6 + x^5/120 - x^7/5040 + x^9/362880
  let x2 = a * a
  let x3 = a * x2
  let x5 = x3 * x2
  let x7 = x5 * x2
  let x9 = x7 * x2
  let x11 = x9 * x2
  a - x3 / 6.0 + x5 / 120.0 - x7 / 5040.0 + x9 / 362880.0 - x11 / 39916800.0
}

///|
fn approx_cos(x : Double) -> Double {
  let pi_half = 1.5707963267949
  approx_sin(x + pi_half)
}

///|
pub fn generate(config : SpiralConfig, rng : @types.RNG) -> SpiralResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let cx = config.width.to_double() / 2.0
  let cy = config.height.to_double() / 2.0
  // Archimedean spiral: r = a + b * theta
  // b controls spacing between turns
  let max_theta = config.num_turns.to_double() * 6.28318530717959
  let b = config.spacing.to_double() / 6.28318530717959
  // Step through angle increments
  let steps = (max_theta * 20.0).to_int() // fine enough steps
  let spiral_points : Array[@types.Point] = []
  let mut prev_gx = -1
  let mut prev_gy = -1
  for i in 0..<steps {
    let theta = max_theta * i.to_double() / steps.to_double()
    let r = b * theta
    let fx = cx + r * approx_cos(theta)
    let fy = cy + r * approx_sin(theta)
    let gx = fx.to_int()
    let gy = fy.to_int()
    // Skip if same grid cell as previous
    if gx == prev_gx && gy == prev_gy {
      continue
    }
    prev_gx = gx
    prev_gy = gy
    if not(grid.in_bounds(gx, gy)) {
      continue
    }
    // Carve corridor tile with width
    carve_wide(grid, gx, gy, config.corridor_width)
    spiral_points.push({ x: gx, y: gy })
    // Connect to previous point
    if spiral_points.length() > 1 {
      let prev = spiral_points[spiral_points.length() - 2]
      carve_corridor_line(grid, prev.x, prev.y, gx, gy, config.corridor_width)
    }
  }
  // Place rooms at regular intervals along the spiral
  let mut room_id = 0
  let room_interval = if spiral_points.length() > 10 {
    spiral_points.length() / 10
  } else {
    1
  }
  for i = 0; i < spiral_points.length(); {
    let p = spiral_points[i]
    if rng.rand_bool(config.room_chance) {
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
    continue i + room_interval
  }
  // Place start and goal
  if spiral_points.length() > 0 {
    let start = spiral_points[0]
    grid.set(start.x, start.y, @types.Start)
    let goal = spiral_points[spiral_points.length() - 1]
    grid.set(goal.x, goal.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_wide(
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
fn carve_corridor_line(
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
    carve_wide(grid, x, y, width)
    if x != x1 {
      x = if x < x1 { x + 1 } else { x - 1 }
    } else if y != y1 {
      y = if y < y1 { y + 1 } else { y - 1 }
    }
  }
  carve_wide(grid, x1, y1, width)
}
