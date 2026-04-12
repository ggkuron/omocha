///|
pub(all) struct SpaceStationConfig {
  width : Int
  height : Int
  depth : Int
  ring_width : Int
  num_spokes : Int
}

///|
pub fn SpaceStationConfig::default() -> SpaceStationConfig {
  { width: 40, height: 40, depth: 3, ring_width: 3, num_spokes: 6 }
}

///|
pub(all) struct SpaceStationResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
fn isqrt(n : Int) -> Int {
  if n <= 0 {
    return 0
  }
  let mut x = n
  let mut y = (x + 1) / 2
  while y < x {
    x = y
    y = (x + n / x) / 2
  }
  x
}

///|
pub fn generate(
  config : SpaceStationConfig,
  rng : @types.RNG
) -> SpaceStationResult {
  let width = config.width
  let height = config.height
  let depth_count = config.depth
  let grid = @types.Grid3D::make(width, height, depth_count, @types.Empty)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  let cx = width / 2
  let cy = height / 2
  // Calculate ring parameters
  let outer_radius = if width < height { width / 2 - 2 } else { height / 2 - 2 }
  let inner_radius = outer_radius - config.ring_width
  let hub_radius = 3
  // For each layer, draw the ring structure
  for z in 0..<depth_count {
    // Draw ring (donut shape)
    for y in 0..<height {
      for x in 0..<width {
        let dx = x - cx
        let dy = y - cy
        let dist_sq = dx * dx + dy * dy
        let dist = isqrt(dist_sq)
        // Hub column at center
        if dist <= hub_radius {
          grid.set(x, y, z, @types.Floor)
        }
        // Ring perimeter
        if dist >= inner_radius && dist <= outer_radius {
          grid.set(x, y, z, @types.Floor)
        }
      }
    }
    // Draw radial spokes from hub to ring
    for spoke in 0..<config.num_spokes {
      // Calculate spoke angle direction
      // Use fixed direction vectors for each spoke
      let angle_num = spoke
      let angle_den = config.num_spokes
      // Pre-compute direction: approximate with integer math
      // We distribute spokes evenly - use lookup for common cases
      let mut sdx = 0
      let mut sdy = 0
      // Map spoke index to direction
      let fraction = angle_num * 8 / angle_den
      match fraction {
        0 => { sdx = 1; sdy = 0 }
        1 => { sdx = 1; sdy = 1 }
        2 => { sdx = 0; sdy = 1 }
        3 => { sdx = -1; sdy = 1 }
        4 => { sdx = -1; sdy = 0 }
        5 => { sdx = -1; sdy = -1 }
        6 => { sdx = 0; sdy = -1 }
        7 => { sdx = 1; sdy = -1 }
        _ => { sdx = 1; sdy = 0 }
      }
      // Draw corridor along spoke direction
      let mut sx = cx
      let mut sy = cy
      let max_steps = outer_radius + 2
      for _step in 0..<max_steps {
        if sx >= 0 && sx < width && sy >= 0 && sy < height {
          grid.set(sx, sy, z, @types.Corridor)
          // Widen spoke slightly
          if sx + 1 < width {
            grid.set(sx + 1, sy, z, @types.Corridor)
          }
          if sy + 1 < height {
            grid.set(sx, sy + 1, z, @types.Corridor)
          }
        }
        sx += sdx
        sy += sdy
      }
    }
    // Place rooms at ring perimeter where spokes meet ring
    for spoke in 0..<config.num_spokes {
      let fraction = spoke * 8 / config.num_spokes
      let mut sdx = 0
      let mut sdy = 0
      match fraction {
        0 => { sdx = 1; sdy = 0 }
        1 => { sdx = 1; sdy = 1 }
        2 => { sdx = 0; sdy = 1 }
        3 => { sdx = -1; sdy = 1 }
        4 => { sdx = -1; sdy = 0 }
        5 => { sdx = -1; sdy = -1 }
        6 => { sdx = 0; sdy = -1 }
        7 => { sdx = 1; sdy = -1 }
        _ => { sdx = 1; sdy = 0 }
      }
      // Room position at ring
      let mid_r = (inner_radius + outer_radius) / 2
      let rx = cx + sdx * mid_r
      let ry = cy + sdy * mid_r
      let room_w = rng.rand_range(3, 6)
      let room_h = rng.rand_range(3, 6)
      let room_x = rx - room_w / 2
      let room_y = ry - room_h / 2
      // Carve room
      if room_x >= 0 &&
        room_y >= 0 &&
        room_x + room_w < width &&
        room_y + room_h < height {
        for dy in 0..<room_h {
          for dx in 0..<room_w {
            grid.set(room_x + dx, room_y + dy, z, @types.Floor)
          }
        }
        // Place door at room entrance
        grid.set(rx, ry, z, @types.Door)
        rooms.push(
          {
            rect: { x: room_x, y: room_y, w: room_w, h: room_h },
            id: room_id,
          },
        )
        room_id += 1
      }
    }
    // Mark hub center
    grid.set(cx, cy, z, @types.Start)
  }
  // Hub room
  rooms.push(
    {
      rect: { x: cx - hub_radius, y: cy - hub_radius, w: hub_radius * 2 + 1, h: hub_radius * 2 + 1 },
      id: room_id,
    },
  )
  ignore(room_id)
  { grid, rooms }
}
