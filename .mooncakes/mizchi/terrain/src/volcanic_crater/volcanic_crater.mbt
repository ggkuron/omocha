///|
pub(all) struct VolcanicCraterConfig {
  width : Int
  height : Int
  max_height : Int
  crater_ratio : Double
  vent_chance : Double
}

///|
pub fn VolcanicCraterConfig::default() -> VolcanicCraterConfig {
  {
    width: 48,
    height: 48,
    max_height: 8,
    crater_ratio: 0.35,
    vent_chance: 0.2,
  }
}

///|
pub(all) struct VolcanicCraterResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : VolcanicCraterConfig,
  rng : @types.RNG
) -> VolcanicCraterResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  let cx = w / 2
  let cy = h / 2
  // Elliptical volcano shape
  let rx = w / 2 - 2 // x radius
  let ry = h / 2 - 2 // y radius
  let crater_rx = (rx.to_double() * config.crater_ratio).to_int()
  let crater_ry = (ry.to_double() * config.crater_ratio).to_int()
  // Rim radius (where height is max)
  let rim_rx = crater_rx + (rx - crater_rx) / 3
  let rim_ry = crater_ry + (ry - crater_ry) / 3
  // Build the volcano height field
  for y in 0..<h {
    for x in 0..<w {
      let dx = x - cx
      let dy = y - cy
      // Normalized distance from center using ellipse formula
      let ex = dx.to_double() / rx.to_double()
      let ey = dy.to_double() / ry.to_double()
      let dist_norm = sqrt_approx(ex * ex + ey * ey) // 0 at center, 1 at edge
      if dist_norm > 1.0 {
        // Outside the volcano
        continue
      }
      // Normalized crater distance
      let crater_ex = dx.to_double() / rim_rx.to_double()
      let crater_ey = dy.to_double() / rim_ry.to_double()
      let crater_dist = sqrt_approx(crater_ex * crater_ex + crater_ey * crater_ey)
      // Add noise for realism
      let noise = (rng.next_int(3) - 1).to_double() * 0.05
      let noisy_dist = dist_norm + noise
      let cell_h = if crater_dist < 0.7 {
        // Inside crater - descending to lava lake
        // Lava lake at the very center
        let inner_ratio = crater_dist / 0.7
        let lake_h = (inner_ratio * 2.0).to_int()
        if lake_h < 1 { 0 } else { lake_h }
      } else if crater_dist < 1.0 {
        // Inner crater wall - steep rise to rim
        let wall_t = (crater_dist - 0.7) / 0.3
        (wall_t * config.max_height.to_double()).to_int()
      } else {
        // Outer slopes - descending from rim to edge
        let outer_t = (noisy_dist - rim_rx.to_double() / rx.to_double()) /
          (1.0 - rim_rx.to_double() / rx.to_double())
        let slope_h = config.max_height -
          (outer_t * config.max_height.to_double()).to_int()
        if slope_h < 1 { 1 } else { slope_h }
      }
      let clamped = if cell_h > config.max_height {
        config.max_height
      } else if cell_h < 0 {
        0
      } else {
        cell_h
      }
      // Determine tile type
      if crater_dist < 0.5 {
        // Lava lake at center
        grid.set(x, y, @types.Water)
        grid.set_height(x, y, 0)
      } else if clamped == config.max_height {
        // Rim - walls
        grid.set(x, y, @types.Wall)
        grid.set_height(x, y, clamped)
      } else {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, clamped)
      }
    }
  }
  // Carve vents/caves into the inner crater walls
  let mut room_id = 0
  let num_vents = 4 + rng.next_int(5) // 4-8 potential vents
  for vi in 0..<num_vents {
    if rng.rand_bool(config.vent_chance) {
      // Pick a direction for the vent
      let angle = vi.to_double() * 6.28318 / num_vents.to_double() +
        rng.next_int(100).to_double() * 0.005
      // Place vent between crater and rim
      let vent_r = (rim_rx.to_double() * 0.85).to_int()
      let vx = cx + (vent_r.to_double() * cos_approx(angle)).to_int()
      let vy = cy + (vent_r.to_double() * sin_approx(angle)).to_int()
      // Carve a small cave
      let cave_size = 2 + rng.next_int(2) // 2-3
      let mut min_x = w
      let mut min_y = h
      let mut max_x = 0
      let mut max_y = 0
      for dy in -cave_size..=cave_size {
        for dx in -cave_size..=cave_size {
          if dx * dx + dy * dy <= cave_size * cave_size {
            let px = vx + dx
            let py = vy + dy
            if px >= 1 && px < w - 1 && py >= 1 && py < h - 1 {
              grid.set(px, py, @types.Floor)
              let vent_h = config.max_height / 2 + rng.next_int(2)
              grid.set_height(px, py, vent_h)
              if px < min_x { min_x = px }
              if py < min_y { min_y = py }
              if px > max_x { max_x = px }
              if py > max_y { max_y = py }
            }
          }
        }
      }
      if max_x >= min_x && max_y >= min_y {
        rooms.push(
          {
            rect: {
              x: min_x,
              y: min_y,
              w: max_x - min_x + 1,
              h: max_y - min_y + 1,
            },
            id: room_id,
          },
        )
        room_id += 1
      }
      // Carve a tunnel from the vent outward through the rim
      let tunnel_len = 3 + rng.next_int(4)
      for ti in 1..=tunnel_len {
        let tx = vx +
          (ti.to_double() * cos_approx(angle + 3.14159)).to_int()
        let ty = vy +
          (ti.to_double() * sin_approx(angle + 3.14159)).to_int()
        if grid.in_bounds(tx, ty) {
          grid.set(tx, ty, @types.Corridor)
          let tunnel_h = config.max_height / 2 +
            ti * (config.max_height / 2) / tunnel_len
          let th_clamped = if tunnel_h > config.max_height {
            config.max_height
          } else {
            tunnel_h
          }
          grid.set_height(tx, ty, th_clamped)
        }
      }
    }
  }
  // Connect adjacent vents with corridors along the crater wall
  for i in 0..<(rooms.length() - 1) {
    let from_center = rooms[i].rect.center()
    let to_center = rooms[i + 1].rect.center()
    carve_arc_corridor(grid, from_center, to_center, cx, cy)
  }
  // Place Start on outer slope, Goal near lava lake edge
  let start_x = cx + rim_rx + 2
  let start_y = cy
  if grid.in_bounds(start_x, start_y) {
    grid.set(start_x, start_y, @types.Start)
  }
  // Goal near crater edge
  let goal_x = cx + crater_rx + 1
  let goal_y = cy
  if grid.in_bounds(goal_x, goal_y) {
    grid.set(goal_x, goal_y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_arc_corridor(
  grid : @types.HeightGrid,
  from : @types.Point,
  to : @types.Point,
  cx : Int,
  cy : Int
) -> Unit {
  // Walk from `from` to `to` along the arc around (cx, cy)
  let steps = abs(from.x - to.x) + abs(from.y - to.y)
  if steps == 0 {
    return
  }
  for s in 0..=steps {
    let t = s.to_double() / steps.to_double()
    let ix = from.x + ((to.x - from.x).to_double() * t).to_int()
    let iy = from.y + ((to.y - from.y).to_double() * t).to_int()
    // Push slightly outward from center for arc effect
    let dx = ix - cx
    let dy = iy - cy
    let dist = isqrt(dx * dx + dy * dy)
    let push = if dist > 0 { 1 } else { 0 }
    let push_x = if dx > 0 { push } else if dx < 0 { -push } else { 0 }
    let push_y = if dy > 0 { push } else if dy < 0 { -push } else { 0 }
    let ax = ix + push_x
    let ay = iy + push_y
    if grid.in_bounds(ax, ay) {
      let tile = grid.get(ax, ay)
      if tile == @types.Wall || tile == @types.Empty {
        grid.set(ax, ay, @types.Corridor)
      }
      // Preserve existing height or set mid height
      let cur_h = grid.get_height(ax, ay)
      if cur_h == 0 {
        grid.set_height(ax, ay, grid.max_height / 2)
      }
    }
  }
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
  // Newton's method iterations
  for _ in 0..<10 {
    guess = (guess + x / guess) / 2.0
  }
  guess
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
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
