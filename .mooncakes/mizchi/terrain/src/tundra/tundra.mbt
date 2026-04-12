///|
pub(all) struct TundraConfig {
  width : Int
  height : Int
  num_outposts : Int
  tunnel_chance : Double
}

///|
pub fn TundraConfig::default() -> TundraConfig {
  { width: 50, height: 30, num_outposts: 6, tunnel_chance: 0.5 }
}

///|
pub(all) struct TundraResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : TundraConfig, rng : @types.RNG) -> TundraResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Floor)
  let rooms : Array[@types.Room] = []
  let outpost_centers : Array[@types.Point] = []
  // Step 1: Scatter ice formations (Wall clusters)
  for y in 0..<h {
    for x in 0..<w {
      if rng.rand_bool(0.08) {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Cluster ice with cellular automata
  for _ in 0..<2 {
    let old = grid.clone()
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let wall_count = old.count_neighbors(x, y, @types.Wall)
        if old.get(x, y) == @types.Wall {
          if wall_count < 1 {
            grid.set(x, y, @types.Floor)
          }
        } else if wall_count >= 5 {
          grid.set(x, y, @types.Wall)
        }
      }
    }
  }
  // Step 2: Place outposts (small walled rooms)
  let mut room_id = 0
  for _ in 0..<config.num_outposts {
    let rw = rng.rand_range(4, 7)
    let rh = rng.rand_range(4, 6)
    let rx = rng.rand_range(2, w - rw - 2)
    let ry = rng.rand_range(2, h - rh - 2)
    // Check that outpost doesn't overlap too much with existing ones
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    let mut overlaps = false
    for existing in rooms {
      if rect.intersects(existing.rect) {
        overlaps = true
        break
      }
    }
    if overlaps {
      continue
    }
    let center : @types.Point = { x: rx + rw / 2, y: ry + rh / 2 }
    outpost_centers.push(center)
    // Build walls around the outpost
    for cx in rx..<(rx + rw) {
      if grid.in_bounds(cx, ry) {
        grid.set(cx, ry, @types.Wall)
      }
      if grid.in_bounds(cx, ry + rh - 1) {
        grid.set(cx, ry + rh - 1, @types.Wall)
      }
    }
    for cy in ry..<(ry + rh) {
      if grid.in_bounds(rx, cy) {
        grid.set(rx, cy, @types.Wall)
      }
      if grid.in_bounds(rx + rw - 1, cy) {
        grid.set(rx + rw - 1, cy, @types.Wall)
      }
    }
    // Interior is Floor
    for cy in (ry + 1)..<(ry + rh - 1) {
      for cx in (rx + 1)..<(rx + rw - 1) {
        if grid.in_bounds(cx, cy) {
          grid.set(cx, cy, @types.Floor)
        }
      }
    }
    // Add a door entrance
    let side = rng.next_int(4)
    match side {
      0 => {
        let dx = rx + rw / 2
        if grid.in_bounds(dx, ry) {
          grid.set(dx, ry, @types.Door)
        }
      }
      1 => {
        let dx = rx + rw / 2
        if grid.in_bounds(dx, ry + rh - 1) {
          grid.set(dx, ry + rh - 1, @types.Door)
        }
      }
      2 => {
        let dy = ry + rh / 2
        if grid.in_bounds(rx, dy) {
          grid.set(rx, dy, @types.Door)
        }
      }
      _ => {
        let dy = ry + rh / 2
        if grid.in_bounds(rx + rw - 1, dy) {
          grid.set(rx + rw - 1, dy, @types.Door)
        }
      }
    }
    rooms.push({ rect, id: room_id })
    room_id += 1
  }
  // Step 3: Connect outposts with underground tunnels (Corridor)
  if outpost_centers.length() > 1 {
    // MST connection
    let n = outpost_centers.length()
    let connected = Array::make(n, false)
    connected[0] = true
    for _ in 1..<n {
      let mut best_from = 0
      let mut best_to = 0
      let mut best_dist = 999999
      for fi in 0..<n {
        if not(connected[fi]) {
          continue
        }
        for ti in 0..<n {
          if connected[ti] {
            continue
          }
          let d = dist_sq(outpost_centers[fi], outpost_centers[ti])
          if d < best_dist {
            best_dist = d
            best_from = fi
            best_to = ti
          }
        }
      }
      connected[best_to] = true
      // Always carve MST tunnel
      carve_tunnel(grid, outpost_centers[best_from], outpost_centers[best_to])
    }
    // Add extra tunnels with tunnel_chance probability
    for fi in 0..<n {
      for ti in (fi + 2)..<n {
        if rng.rand_bool(config.tunnel_chance * 0.3) {
          carve_tunnel(grid, outpost_centers[fi], outpost_centers[ti])
        }
      }
    }
  }
  // Step 4: Place supply caches (Key) near outposts
  for center in outpost_centers {
    // Place a Key tile near the outpost
    let kx = center.x + rng.rand_range(-3, 4)
    let ky = center.y + rng.rand_range(-3, 4)
    if grid.in_bounds(kx, ky) {
      let current = grid.get(kx, ky)
      if current == @types.Floor || current == @types.Corridor {
        grid.set(kx, ky, @types.Key)
      }
    }
  }
  // Place Start and Goal
  if outpost_centers.length() > 0 {
    let sc = outpost_centers[0]
    let sp = find_walkable(grid, sc.x, sc.y)
    grid.set(sp.x, sp.y, @types.Start)
    let gc = outpost_centers[outpost_centers.length() - 1]
    let gp = find_walkable(grid, gc.x, gc.y)
    if gp.x != sp.x || gp.y != sp.y {
      grid.set(gp.x, gp.y, @types.Goal)
    }
  }
  { grid, rooms }
}

///|
fn dist_sq(a : @types.Point, b : @types.Point) -> Int {
  let dx = a.x - b.x
  let dy = a.y - b.y
  dx * dx + dy * dy
}

///|
fn carve_tunnel(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if grid.in_bounds(x, y) {
      let current = grid.get(x, y)
      if current == @types.Wall || current == @types.Floor {
        grid.set(x, y, @types.Corridor)
      }
    }
    if x != to.x {
      if x < to.x {
        x += 1
      } else {
        x -= 1
      }
    } else if y != to.y {
      if y < to.y {
        y += 1
      } else {
        y -= 1
      }
    }
  }
  if grid.in_bounds(to.x, to.y) {
    let current = grid.get(to.x, to.y)
    if current == @types.Wall || current == @types.Floor {
      grid.set(to.x, to.y, @types.Corridor)
    }
  }
}

///|
fn find_walkable(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let adx = if dx < 0 { -dx } else { dx }
        let ady = if dy < 0 { -dy } else { dy }
        if adx + ady != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) {
          let t = grid.get(x, y)
          if t == @types.Floor || t == @types.Corridor {
            return { x, y }
          }
        }
      }
    }
  }
  { x: cx, y: cy }
}
