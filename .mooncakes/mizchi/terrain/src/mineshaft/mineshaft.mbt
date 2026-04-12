///|
pub(all) struct MineshaftConfig {
  width : Int
  height : Int
  max_height : Int
  num_levels : Int
  tunnel_count : Int
}

///|
pub fn MineshaftConfig::default() -> MineshaftConfig {
  { width: 48, height: 48, max_height: 8, num_levels: 4, tunnel_count: 4 }
}

///|
pub(all) struct MineshaftResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : MineshaftConfig, rng : @types.RNG) -> MineshaftResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Fill everything with wall at max height (solid rock)
  for y in 0..<h {
    for x in 0..<w {
      grid.set_height(x, y, config.max_height)
    }
  }
  // Central elevator shaft
  let shaft_cx = w / 2
  let shaft_cy = h / 2
  let shaft_radius = 2
  // Carve elevator shaft (vertical column, low height = deep)
  for dy in -shaft_radius..=shaft_radius {
    for dx in -shaft_radius..=shaft_radius {
      let px = shaft_cx + dx
      let py = shaft_cy + dy
      if px >= 0 && px < w && py >= 0 && py < h {
        // Shaft walls on the border, floor inside
        let is_border = abs(dx) == shaft_radius || abs(dy) == shaft_radius
        if is_border {
          grid.set(px, py, @types.Wall)
          grid.set_height(px, py, config.max_height)
        } else {
          grid.set(px, py, @types.Floor)
          grid.set_height(px, py, 0)
        }
      }
    }
  }
  // Create shaft room
  rooms.push(
    {
      rect: {
        x: shaft_cx - shaft_radius,
        y: shaft_cy - shaft_radius,
        w: shaft_radius * 2 + 1,
        h: shaft_radius * 2 + 1,
      },
      id: room_id,
    },
  )
  room_id += 1
  // Generate mine levels at different heights
  // Each level radiates tunnels from the shaft in cardinal/diagonal directions
  let level_heights : Array[Int] = []
  for i in 0..<config.num_levels {
    let level_h = (i + 1) * config.max_height / (config.num_levels + 1)
    level_heights.push(level_h)
  }
  // For each level, create a landing room and radiating tunnels
  for level_idx in 0..<config.num_levels {
    let level_h = level_heights[level_idx]
    // Landing room at shaft exit for this level
    // Place the landing at a different side of the shaft for each level
    let side = level_idx % 4
    let landing_cx = match side {
      0 => shaft_cx + shaft_radius + 2
      1 => shaft_cx - shaft_radius - 2
      2 => shaft_cx
      _ => shaft_cx
    }
    let landing_cy = match side {
      0 => shaft_cy
      1 => shaft_cy
      2 => shaft_cy - shaft_radius - 2
      _ => shaft_cy + shaft_radius + 2
    }
    // Carve landing room
    let landing_size = 3
    let half_l = landing_size / 2
    for dy in -half_l..=half_l {
      for dx in -half_l..=half_l {
        let px = landing_cx + dx
        let py = landing_cy + dy
        if px >= 0 && px < w && py >= 0 && py < h {
          let is_border = abs(dx) == half_l || abs(dy) == half_l
          if is_border {
            grid.set(px, py, @types.Wall)
            grid.set_height(px, py, level_h + 1)
          } else {
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, level_h)
          }
        }
      }
    }
    rooms.push(
      {
        rect: {
          x: landing_cx - half_l,
          y: landing_cy - half_l,
          w: landing_size,
          h: landing_size,
        },
        id: room_id,
      },
    )
    room_id += 1
    // Connect landing to shaft via corridor
    carve_corridor(
      grid, shaft_cx, shaft_cy, landing_cx, landing_cy, level_h, w, h,
    )
    // Radiate tunnels from this level
    let tunnel_dirs = generate_tunnel_directions(config.tunnel_count, rng)
    for dir in tunnel_dirs {
      let tunnel_len = rng.rand_range(8, 18)
      let mut tx = landing_cx
      let mut ty = landing_cy
      for _step in 0..<tunnel_len {
        tx += dir.0
        ty += dir.1
        // Occasionally wiggle
        if rng.rand_bool(0.15) {
          if dir.0 == 0 {
            tx += rng.rand_range(-1, 2)
          } else {
            ty += rng.rand_range(-1, 2)
          }
        }
        if tx < 1 || tx >= w - 1 || ty < 1 || ty >= h - 1 {
          break
        }
        // Carve tunnel (1-wide corridor)
        grid.set(tx, ty, @types.Corridor)
        grid.set_height(tx, ty, level_h)
        // Support beams every 4 steps
        if _step % 4 == 0 && _step > 0 {
          // Place wall support beams on sides
          let beam_dx = if dir.0 == 0 { 1 } else { 0 }
          let beam_dy = if dir.1 == 0 { 1 } else { 0 }
          let bx1 = tx + beam_dx
          let by1 = ty + beam_dy
          if bx1 >= 0 && bx1 < w && by1 >= 0 && by1 < h {
            if grid.get(bx1, by1) == @types.Wall &&
              grid.get_height(bx1, by1) == config.max_height {
              grid.set_height(bx1, by1, level_h + 1)
            }
          }
          let bx2 = tx - beam_dx
          let by2 = ty - beam_dy
          if bx2 >= 0 && bx2 < w && by2 >= 0 && by2 < h {
            if grid.get(bx2, by2) == @types.Wall &&
              grid.get_height(bx2, by2) == config.max_height {
              grid.set_height(bx2, by2, level_h + 1)
            }
          }
        }
        // Scatter ore veins (Key tiles) along tunnels
        if rng.rand_bool(0.08) {
          // Place ore on an adjacent wall
          let ore_dirs : Array[(Int, Int)] = [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
          ]
          let od = rng.rand_pick(ore_dirs)
          let ox = tx + od.0
          let oy = ty + od.1
          if ox >= 0 && ox < w && oy >= 0 && oy < h {
            if grid.get(ox, oy) == @types.Wall {
              grid.set(ox, oy, @types.Key)
              grid.set_height(ox, oy, level_h)
            }
          }
        }
      }
      // At end of some tunnels, carve a small chamber
      if rng.rand_bool(0.6) {
        let chamber_size = rng.rand_range(2, 4)
        let half_c = chamber_size / 2
        for dy in -half_c..=half_c {
          for dx in -half_c..=half_c {
            let px = tx + dx
            let py = ty + dy
            if px >= 1 && px < w - 1 && py >= 1 && py < h - 1 {
              let is_border = abs(dx) == half_c || abs(dy) == half_c
              if is_border {
                if grid.get(px, py) == @types.Wall {
                  grid.set_height(px, py, level_h + 1)
                }
              } else {
                grid.set(px, py, @types.Floor)
                grid.set_height(px, py, level_h)
              }
            }
          }
        }
        rooms.push(
          {
            rect: {
              x: tx - half_c,
              y: ty - half_c,
              w: chamber_size + 1,
              h: chamber_size + 1,
            },
            id: room_id,
          },
        )
        room_id += 1
      }
    }
  }
  // Place Start at shaft center, Goal at deepest tunnel end
  grid.set(shaft_cx, shaft_cy, @types.Start)
  grid.set_height(shaft_cx, shaft_cy, 0)
  // Find a Floor cell far from center for Goal
  let mut best_gx = shaft_cx
  let mut best_gy = shaft_cy
  let mut best_dist = 0
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let tile = grid.get(x, y)
      if tile == @types.Floor || tile == @types.Corridor {
        let dx = x - shaft_cx
        let dy = y - shaft_cy
        let d = dx * dx + dy * dy
        if d > best_dist {
          best_dist = d
          best_gx = x
          best_gy = y
        }
      }
    }
  }
  grid.set(best_gx, best_gy, @types.Goal)
  { grid, rooms }
}

///|
fn generate_tunnel_directions(
  count : Int,
  rng : @types.RNG
) -> Array[(Int, Int)] {
  let all_dirs : Array[(Int, Int)] = [
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
    (1, 1),
    (-1, 1),
    (1, -1),
    (-1, -1),
  ]
  rng.rand_shuffle(all_dirs)
  let result : Array[(Int, Int)] = []
  let n = count.min(all_dirs.length())
  for i in 0..<n {
    result.push(all_dirs[i])
  }
  result
}

///|
fn carve_corridor(
  grid : @types.HeightGrid,
  x0 : Int,
  y0 : Int,
  x1 : Int,
  y1 : Int,
  level_h : Int,
  w : Int,
  h : Int
) -> Unit {
  let mut x = x0
  let mut y = y0
  // Horizontal first, then vertical
  while x != x1 {
    if x >= 0 && x < w && y >= 0 && y < h {
      let cur = grid.get(x, y)
      if cur == @types.Wall {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, level_h)
      }
    }
    x = x + (if x1 > x { 1 } else { -1 })
  }
  while y != y1 {
    if x >= 0 && x < w && y >= 0 && y < h {
      let cur = grid.get(x, y)
      if cur == @types.Wall {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, level_h)
      }
    }
    y = y + (if y1 > y { 1 } else { -1 })
  }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}
