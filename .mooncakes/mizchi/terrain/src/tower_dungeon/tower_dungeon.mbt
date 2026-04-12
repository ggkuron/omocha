///|
pub(all) struct TowerDungeonConfig {
  width : Int
  height : Int
  max_height : Int
  floors : Int
  room_size : Int
}

///|
pub fn TowerDungeonConfig::default() -> TowerDungeonConfig {
  { width: 40, height: 40, max_height: 8, floors: 4, room_size: 4 }
}

///|
pub(all) struct TowerDungeonResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : TowerDungeonConfig,
  rng : @types.RNG
) -> TowerDungeonResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  let cx = w / 2
  let cy = h / 2
  // Compute tower radii for each floor (tapering upward)
  // Floor 0 is the base (largest), floor (floors-1) is the top (smallest)
  let base_radius = if w < h { w / 2 - 2 } else { h / 2 - 2 }
  let min_radius = config.room_size + 2
  let mut room_id = 0
  // Build each floor as a ring
  for floor_idx in 0..<config.floors {
    let t = floor_idx.to_double() / config.floors.to_double()
    let radius = base_radius -
      ((base_radius - min_radius).to_double() * t).to_int()
    let floor_height = floor_idx * config.max_height / config.floors + 1
    let inner_radius = if radius > config.room_size + 2 {
      radius - config.room_size
    } else {
      2
    }
    // Draw the circular floor ring
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let dist_sq = dx * dx + dy * dy
        let r_sq = radius * radius
        let ir_sq = inner_radius * inner_radius
        let px = cx + dx
        let py = cy + dy
        if px >= 0 && px < w && py >= 0 && py < h {
          if dist_sq <= r_sq && dist_sq >= ir_sq {
            // This cell is in the ring for this floor
            let cur_h = grid.get_height(px, py)
            if floor_height > cur_h {
              grid.set(px, py, @types.Floor)
              grid.set_height(px, py, floor_height)
            }
          }
        }
      }
    }
    // Outer wall ring
    for dy in -(radius + 1)..=(radius + 1) {
      for dx in -(radius + 1)..=(radius + 1) {
        let dist_sq = dx * dx + dy * dy
        let outer_sq = (radius + 1) * (radius + 1)
        let r_sq = radius * radius
        let px = cx + dx
        let py = cy + dy
        if px >= 0 && px < w && py >= 0 && py < h {
          if dist_sq <= outer_sq && dist_sq > r_sq {
            let cur_tile = grid.get(px, py)
            if cur_tile == @types.Empty {
              grid.set(px, py, @types.Wall)
              grid.set_height(px, py, floor_height)
            }
          }
        }
      }
    }
    // Inner wall ring (stairwell boundary)
    for dy in -(inner_radius)..=(inner_radius) {
      for dx in -(inner_radius)..=(inner_radius) {
        let dist_sq = dx * dx + dy * dy
        let ir_sq = inner_radius * inner_radius
        let inner_wall_sq = (inner_radius - 1) * (inner_radius - 1)
        let px = cx + dx
        let py = cy + dy
        if px >= 0 && px < w && py >= 0 && py < h {
          if dist_sq <= ir_sq && dist_sq > inner_wall_sq {
            let cur_tile = grid.get(px, py)
            if cur_tile == @types.Empty || cur_tile == @types.Floor {
              grid.set(px, py, @types.Wall)
              grid.set_height(px, py, floor_height)
            }
          }
        }
      }
    }
    // Carve rooms in the ring at cardinal + diagonal directions
    let num_rooms_this_floor = 4 + rng.next_int(3) // 4-6 rooms
    let mid_radius = (radius + inner_radius) / 2
    for ri in 0..<num_rooms_this_floor {
      // Distribute rooms around the ring
      let angle = ri.to_double() * 6.28318 / num_rooms_this_floor.to_double()
      let room_cx = cx + (mid_radius.to_double() * cos_approx(angle)).to_int()
      let room_cy = cy + (mid_radius.to_double() * sin_approx(angle)).to_int()
      let rs = config.room_size
      let rx = room_cx - rs / 2
      let ry = room_cy - rs / 2
      // Carve room
      let mut valid = true
      for dy in 0..<rs {
        for dx in 0..<rs {
          let px = rx + dx
          let py = ry + dy
          if px < 1 || px >= w - 1 || py < 1 || py >= h - 1 {
            valid = false
          }
        }
      }
      if valid {
        for dy in 0..<rs {
          for dx in 0..<rs {
            let px = rx + dx
            let py = ry + dy
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, floor_height)
          }
        }
        // Add walls around the room
        for dy in -1..=rs {
          for dx in -1..=rs {
            let px = rx + dx
            let py = ry + dy
            if px >= 0 && px < w && py >= 0 && py < h {
              if dx == -1 || dx == rs || dy == -1 || dy == rs {
                if grid.get(px, py) != @types.Floor {
                  grid.set(px, py, @types.Wall)
                  grid.set_height(px, py, floor_height)
                }
              }
            }
          }
        }
        // Place a door at one edge of the room
        let door_side = rng.next_int(4)
        let door_x = match door_side {
          0 => rx - 1 // left
          1 => rx + rs // right
          2 => rx + rs / 2 // top
          _ => rx + rs / 2 // bottom
        }
        let door_y = match door_side {
          0 => ry + rs / 2 // left
          1 => ry + rs / 2 // right
          2 => ry - 1 // top
          _ => ry + rs // bottom
        }
        if door_x >= 0 && door_x < w && door_y >= 0 && door_y < h {
          grid.set(door_x, door_y, @types.Door)
          grid.set_height(door_x, door_y, floor_height)
        }
        rooms.push(
          { rect: { x: rx, y: ry, w: rs, h: rs }, id: room_id },
        )
        room_id += 1
      }
    }
  }
  // Carve spiral staircase in the center stairwell
  carve_spiral_staircase(grid, cx, cy, min_radius - 2, config)
  // Place Start at bottom floor, Goal at top floor
  if rooms.length() > 0 {
    let start_center = rooms[0].rect.center()
    grid.set(start_center.x, start_center.y, @types.Start)
    let goal_center = rooms[rooms.length() - 1].rect.center()
    grid.set(goal_center.x, goal_center.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_spiral_staircase(
  grid : @types.HeightGrid,
  cx : Int,
  cy : Int,
  stair_radius : Int,
  config : TowerDungeonConfig
) -> Unit {
  let sr = if stair_radius < 1 { 1 } else { stair_radius }
  // Spiral staircase: walk around the center in a spiral, increasing height
  let total_steps = config.floors * 8 // steps per full revolution
  let height_per_step = (config.max_height.to_double()) /
    total_steps.to_double()
  for step in 0..<total_steps {
    let angle = step.to_double() * 6.28318 / 8.0
    let r = sr.to_double() * 0.8
    let sx = cx + (r * cos_approx(angle)).to_int()
    let sy = cy + (r * sin_approx(angle)).to_int()
    let stair_h = (step.to_double() * height_per_step).to_int() + 1
    let clamped_h = if stair_h > config.max_height {
      config.max_height
    } else {
      stair_h
    }
    if grid.in_bounds(sx, sy) {
      grid.set(sx, sy, @types.Corridor)
      grid.set_height(sx, sy, clamped_h)
    }
    // Also carve adjacent cell for wider staircase
    let sx2 = cx + ((r * 0.6) * cos_approx(angle + 0.3)).to_int()
    let sy2 = cy + ((r * 0.6) * sin_approx(angle + 0.3)).to_int()
    if grid.in_bounds(sx2, sy2) {
      grid.set(sx2, sy2, @types.Corridor)
      grid.set_height(sx2, sy2, clamped_h)
    }
  }
}

///|
fn cos_approx(angle : Double) -> Double {
  // Simple cosine approximation using Taylor series
  // Normalize angle to [-pi, pi]
  let pi = 3.14159265358979
  let mut a = angle
  while a > pi {
    a = a - 2.0 * pi
  }
  while a < -pi {
    a = a + 2.0 * pi
  }
  let a2 = a * a
  let a4 = a2 * a2
  let a6 = a4 * a2
  let a8 = a4 * a4
  1.0 - a2 / 2.0 + a4 / 24.0 - a6 / 720.0 + a8 / 40320.0
}

///|
fn sin_approx(angle : Double) -> Double {
  // sin(x) = cos(x - pi/2)
  cos_approx(angle - 1.5707963267949)
}
