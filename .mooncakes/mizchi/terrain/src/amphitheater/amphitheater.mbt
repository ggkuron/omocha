///|
pub(all) struct AmphitheaterConfig {
  width : Int
  height : Int
  max_height : Int
  num_entrances : Int
  arena_ratio : Double
}

///|
pub fn AmphitheaterConfig::default() -> AmphitheaterConfig {
  {
    width: 48,
    height: 48,
    max_height: 6,
    num_entrances: 4,
    arena_ratio: 0.3,
  }
}

///|
pub(all) struct AmphitheaterResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : AmphitheaterConfig,
  rng : @types.RNG
) -> AmphitheaterResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  let cx = w / 2
  let cy = h / 2
  // Elliptical radii for the overall structure
  let outer_rx = w / 2 - 2
  let outer_ry = h / 2 - 2
  // Arena radii (inner open space)
  let arena_rx = (outer_rx.to_double() * config.arena_ratio).to_int()
  let arena_ry = (outer_ry.to_double() * config.arena_ratio).to_int()
  // Number of tiers between arena and outer wall
  let num_tiers = config.max_height
  // Build the tiered seating
  for y in 0..<h {
    for x in 0..<w {
      let dx = x - cx
      let dy = y - cy
      // Elliptical normalized distance
      let ex = dx.to_double() / outer_rx.to_double()
      let ey = dy.to_double() / outer_ry.to_double()
      let dist_norm = sqrt_approx(ex * ex + ey * ey)
      if dist_norm > 1.0 {
        // Outside the structure
        continue
      }
      // Arena normalized distance
      let ax = dx.to_double() / arena_rx.to_double()
      let ay = dy.to_double() / arena_ry.to_double()
      let arena_dist = sqrt_approx(ax * ax + ay * ay)
      if arena_dist < 1.0 {
        // Inside the arena - flat floor at height 0
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, 0)
      } else {
        // Seating tiers - height increases from arena to outer wall
        // Map from arena edge (arena_dist=1) to outer edge (dist_norm=1)
        let tier_t = (dist_norm - config.arena_ratio) / (1.0 - config.arena_ratio)
        let tier_idx = (tier_t * num_tiers.to_double()).to_int()
        let clamped_tier = if tier_idx < 0 {
          0
        } else if tier_idx >= num_tiers {
          num_tiers - 1
        } else {
          tier_idx
        }
        let tier_height = clamped_tier + 1
        // Outer wall ring
        if dist_norm > 0.92 {
          grid.set(x, y, @types.Wall)
          grid.set_height(x, y, config.max_height)
        } else {
          grid.set(x, y, @types.Floor)
          grid.set_height(x, y, tier_height)
        }
      }
    }
  }
  // Arena wall (boundary between arena and first tier)
  for y in 0..<h {
    for x in 0..<w {
      let dx = x - cx
      let dy = y - cy
      let ax_d = dx.to_double() / arena_rx.to_double()
      let ay_d = dy.to_double() / arena_ry.to_double()
      let arena_dist = sqrt_approx(ax_d * ax_d + ay_d * ay_d)
      // Thin wall ring at arena boundary
      if arena_dist >= 0.95 && arena_dist < 1.15 {
        let tile = grid.get(x, y)
        if tile == @types.Floor {
          let cur_h = grid.get_height(x, y)
          if cur_h <= 1 {
            grid.set(x, y, @types.Wall)
            grid.set_height(x, y, 1)
          }
        }
      }
    }
  }
  // Carve entrances (corridors cutting through the tiers)
  let mut room_id = 0
  for ei in 0..<config.num_entrances {
    let angle = ei.to_double() * 6.28318 / config.num_entrances.to_double() +
      rng.next_int(100).to_double() * 0.002
    // Carve a corridor from outer wall to arena
    let corridor_width = 2
    let steps = outer_rx + outer_ry // generous step count
    for s in 0..=steps {
      let t = s.to_double() / steps.to_double()
      let r = outer_rx.to_double() * (1.0 - t) * 0.98
      let base_x = cx +
        (r * cos_approx(angle)).to_int()
      let base_y = cy +
        (r * sin_approx(angle)).to_int()
      // Perpendicular direction for corridor width
      let perp_x = -sin_approx(angle)
      let perp_y = cos_approx(angle)
      for cw in 0..<corridor_width {
        let offset = cw.to_double() - (corridor_width.to_double() - 1.0) / 2.0
        let px = base_x + (perp_x * offset).to_int()
        let py = base_y + (perp_y * offset).to_int()
        if grid.in_bounds(px, py) {
          // Gradually decrease height toward arena
          let corridor_h = (t * config.max_height.to_double()).to_int()
          let ch = config.max_height - corridor_h
          let ch_clamped = if ch < 0 { 0 } else { ch }
          grid.set(px, py, @types.Corridor)
          grid.set_height(px, py, ch_clamped)
        }
      }
    }
    // Place a Door at the outer entrance
    let door_x = cx +
      ((outer_rx.to_double() * 0.92) * cos_approx(angle)).to_int()
    let door_y = cy +
      ((outer_ry.to_double() * 0.92) * sin_approx(angle)).to_int()
    if grid.in_bounds(door_x, door_y) {
      grid.set(door_x, door_y, @types.Door)
      grid.set_height(door_x, door_y, config.max_height - 1)
    }
  }
  // Carve VIP rooms embedded in the tier structure
  let num_vip = 2 + rng.next_int(3) // 2-4 VIP rooms
  for vi in 0..<num_vip {
    // Place VIP rooms at various angles, at mid-tier radius
    let vip_angle = (vi.to_double() + 0.5) * 6.28318 /
      config.num_entrances.to_double() +
      rng.next_int(100).to_double() * 0.01
    let mid_r_x = (outer_rx.to_double() * 0.65).to_int()
    let mid_r_y = (outer_ry.to_double() * 0.65).to_int()
    let vip_cx = cx +
      (mid_r_x.to_double() * cos_approx(vip_angle)).to_int()
    let vip_cy = cy +
      (mid_r_y.to_double() * sin_approx(vip_angle)).to_int()
    let vip_size = 3 + rng.next_int(2) // 3-4
    let vip_rx = vip_cx - vip_size / 2
    let vip_ry = vip_cy - vip_size / 2
    let vip_height = config.max_height / 2 + 1
    // Check bounds
    let mut valid = true
    if vip_rx < 1 || vip_rx + vip_size >= w - 1 {
      valid = false
    }
    if vip_ry < 1 || vip_ry + vip_size >= h - 1 {
      valid = false
    }
    if valid {
      // Carve room interior
      for dy in 0..<vip_size {
        for dx in 0..<vip_size {
          let px = vip_rx + dx
          let py = vip_ry + dy
          if grid.in_bounds(px, py) {
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, vip_height)
          }
        }
      }
      // Surround with walls
      for dy in -1..=vip_size {
        for dx in -1..=vip_size {
          let px = vip_rx + dx
          let py = vip_ry + dy
          if grid.in_bounds(px, py) {
            if dx == -1 || dx == vip_size || dy == -1 || dy == vip_size {
              let tile = grid.get(px, py)
              if tile != @types.Corridor && tile != @types.Door {
                grid.set(px, py, @types.Wall)
                grid.set_height(px, py, vip_height + 1)
              }
            }
          }
        }
      }
      // Place a door facing the arena (toward center)
      let toward_cx = if vip_cx < cx { vip_rx + vip_size } else { vip_rx - 1 }
      let door_dy = vip_ry + vip_size / 2
      if grid.in_bounds(toward_cx, door_dy) {
        grid.set(toward_cx, door_dy, @types.Door)
        grid.set_height(toward_cx, door_dy, vip_height)
      }
      rooms.push(
        {
          rect: { x: vip_rx, y: vip_ry, w: vip_size, h: vip_size },
          id: room_id,
        },
      )
      room_id += 1
    }
  }
  // Place Start at one entrance, Goal in the arena center
  // Start at first entrance
  let start_angle = 0.0
  let start_x = cx +
    ((outer_rx.to_double() * 0.85) * cos_approx(start_angle)).to_int()
  let start_y = cy +
    ((outer_ry.to_double() * 0.85) * sin_approx(start_angle)).to_int()
  if grid.in_bounds(start_x, start_y) {
    grid.set(start_x, start_y, @types.Start)
  }
  // Goal in arena center
  grid.set(cx, cy, @types.Goal)
  grid.set_height(cx, cy, 0)
  { grid, rooms }
}

///|
fn cos_approx(angle : Double) -> Double {
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
  cos_approx(angle - 1.5707963267949)
}

///|
fn sqrt_approx(x : Double) -> Double {
  if x <= 0.0 {
    return 0.0
  }
  let mut guess = x / 2.0
  for _ in 0..<10 {
    guess = (guess + x / guess) / 2.0
  }
  guess
}
