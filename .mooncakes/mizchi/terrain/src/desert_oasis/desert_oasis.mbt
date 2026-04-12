///|
pub(all) struct DesertOasisConfig {
  width : Int
  height : Int
  num_oases : Int
  dune_density : Double
}

///|
pub fn DesertOasisConfig::default() -> DesertOasisConfig {
  { width: 50, height: 30, num_oases: 5, dune_density: 0.2 }
}

///|
pub(all) struct DesertOasisResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : DesertOasisConfig,
  rng : @types.RNG
) -> DesertOasisResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Floor)
  let rooms : Array[@types.Room] = []
  let oasis_centers : Array[@types.Point] = []
  // Step 1: Generate sand dunes (Wall obstacles) using clustered placement
  for y in 0..<h {
    for x in 0..<w {
      if rng.rand_bool(config.dune_density * 0.3) {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Cellular automata to cluster dunes
  for _ in 0..<3 {
    let old = grid.clone()
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let wall_count = old.count_neighbors(x, y, @types.Wall)
        if old.get(x, y) == @types.Wall {
          if wall_count < 2 {
            grid.set(x, y, @types.Floor)
          }
        } else if wall_count >= 5 {
          grid.set(x, y, @types.Wall)
        }
      }
    }
  }
  // Step 2: Place oases (Water surrounded by Corridor)
  let mut room_id = 0
  for _ in 0..<config.num_oases {
    let radius = rng.rand_range(2, 4)
    let cx = rng.rand_range(radius + 2, w - radius - 2)
    let cy = rng.rand_range(radius + 2, h - radius - 2)
    oasis_centers.push({ x: cx, y: cy })
    // Clear area and place water
    let r_sq = radius * radius
    for dy in -(radius + 1)..=(radius + 1) {
      for dx in -(radius + 1)..=(radius + 1) {
        let dist_sq = dx * dx + dy * dy
        let px = cx + dx
        let py = cy + dy
        if grid.in_bounds(px, py) {
          if dist_sq <= r_sq {
            // Inner: water
            grid.set(px, py, @types.Water)
          } else if dist_sq <= (radius + 1) * (radius + 1) {
            // Outer ring: vegetation (Corridor)
            grid.set(px, py, @types.Corridor)
          }
        }
      }
    }
    // Record oasis as room
    rooms.push(
      {
        rect: {
          x: cx - radius,
          y: cy - radius,
          w: radius * 2 + 1,
          h: radius * 2 + 1,
        },
        id: room_id,
      },
    )
    room_id += 1
  }
  // Step 3: Place nomad camps (small rooms with Door) near oases
  for i in 0..<config.num_oases {
    let oc = oasis_centers[i]
    // Place camp offset from oasis
    let offset_x = rng.rand_range(-5, 6)
    let offset_y = rng.rand_range(-5, 6)
    let camp_x = oc.x + offset_x
    let camp_y = oc.y + offset_y
    let camp_w = rng.rand_range(3, 5)
    let camp_h = rng.rand_range(3, 4)
    if camp_x >= 1 && camp_x + camp_w < w - 1 && camp_y >= 1 && camp_y + camp_h < h - 1 {
      // Build camp walls
      for cx in camp_x..<(camp_x + camp_w) {
        grid.set(cx, camp_y, @types.Door)
        grid.set(cx, camp_y + camp_h - 1, @types.Door)
      }
      for cy in camp_y..<(camp_y + camp_h) {
        grid.set(camp_x, cy, @types.Door)
        grid.set(camp_x + camp_w - 1, cy, @types.Door)
      }
      // Interior is Floor
      for cy in (camp_y + 1)..<(camp_y + camp_h - 1) {
        for cx in (camp_x + 1)..<(camp_x + camp_w - 1) {
          grid.set(cx, cy, @types.Floor)
        }
      }
      // Entrance
      let entrance_x = camp_x + camp_w / 2
      grid.set(entrance_x, camp_y + camp_h - 1, @types.Floor)
      rooms.push(
        {
          rect: { x: camp_x, y: camp_y, w: camp_w, h: camp_h },
          id: room_id,
        },
      )
      room_id += 1
    }
  }
  // Step 4: Connect oases with caravan routes (Corridor)
  if oasis_centers.length() > 1 {
    // Build MST to connect all oases
    let n = oasis_centers.length()
    let connected = Array::make(n, false)
    connected[0] = true
    let edges : Array[(Int, Int)] = []
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
          let d = dist_sq(oasis_centers[fi], oasis_centers[ti])
          if d < best_dist {
            best_dist = d
            best_from = fi
            best_to = ti
          }
        }
      }
      connected[best_to] = true
      edges.push((best_from, best_to))
    }
    // Carve caravan routes
    for edge in edges {
      let from = oasis_centers[edge.0]
      let to = oasis_centers[edge.1]
      carve_caravan(grid, from, to, rng)
    }
  }
  // Place Start and Goal
  if oasis_centers.length() > 0 {
    let start_oc = oasis_centers[0]
    let sp = find_floor(grid, start_oc.x, start_oc.y)
    grid.set(sp.x, sp.y, @types.Start)
    let goal_oc = oasis_centers[oasis_centers.length() - 1]
    let gp = find_floor(grid, goal_oc.x, goal_oc.y)
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
fn carve_caravan(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  rng : @types.RNG
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if grid.in_bounds(x, y) {
      let current = grid.get(x, y)
      if current == @types.Floor || current == @types.Wall {
        grid.set(x, y, @types.Corridor)
      }
    }
    // Move toward target with slight wobble
    if rng.rand_bool(0.15) {
      let dir = rng.rand_direction()
      let nx = x + dir.x
      let ny = y + dir.y
      if grid.in_bounds(nx, ny) {
        x = nx
        y = ny
      }
    } else if x != to.x && (y == to.y || rng.rand_bool(0.5)) {
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
}

///|
fn find_floor(
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
