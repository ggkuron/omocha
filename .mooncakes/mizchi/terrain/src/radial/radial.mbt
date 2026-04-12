///|
pub(all) struct RadialConfig {
  width : Int
  height : Int
  num_rings : Int
  sectors_per_ring : Int
  ring_width : Int
  spoke_chance : Double
  gap_chance : Double
}

///|
pub fn RadialConfig::default() -> RadialConfig {
  {
    width: 41,
    height: 41,
    num_rings: 4,
    sectors_per_ring: 8,
    ring_width: 3,
    spoke_chance: 0.6,
    gap_chance: 0.2,
  }
}

///|
pub(all) struct RadialResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
pub fn generate(config : RadialConfig, rng : @types.RNG) -> RadialResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let cx = config.width / 2
  let cy = config.height / 2
  let rooms : Array[@types.Room] = []
  let edges : Array[@types.Edge] = []
  // Precompute sin/cos lookup (integer approximation using fixed sectors)
  // We use a simple approach: for each ring, carve cells within radius range
  // For sectors, use angle-based assignment
  let mut room_id = 0
  // Phase 1: Carve ring sectors as rooms
  // ring_rooms[ring][sector] = room_id or -1
  let ring_rooms = Array::make(
    config.num_rings * config.sectors_per_ring,
    -1,
  )
  for ring in 0..<config.num_rings {
    let inner_r = ring * (config.ring_width + 1) + 2
    let outer_r = inner_r + config.ring_width - 1
    for sector in 0..<config.sectors_per_ring {
      // Decide if this sector has a gap
      if ring > 0 && rng.rand_bool(config.gap_chance) {
        continue
      }
      // Carve cells in this ring-sector
      let mut min_x = config.width
      let mut min_y = config.height
      let mut max_x = 0
      let mut max_y = 0
      let mut carved = false
      for y in 0..<config.height {
        for x in 0..<config.width {
          let dx = x - cx
          let dy = y - cy
          let dist_sq = dx * dx + dy * dy
          if dist_sq >= inner_r * inner_r && dist_sq <= (outer_r + 1) * (outer_r + 1) {
            // Determine sector via atan2-like quadrant mapping
            let s = get_sector(dx, dy, config.sectors_per_ring)
            if s == sector {
              grid.set(x, y, @types.Floor)
              if x < min_x { min_x = x }
              if y < min_y { min_y = y }
              if x > max_x { max_x = x }
              if y > max_y { max_y = y }
              carved = true
            }
          }
        }
      }
      if carved {
        let rect : @types.Rect = {
          x: min_x,
          y: min_y,
          w: max_x - min_x + 1,
          h: max_y - min_y + 1,
        }
        rooms.push({ rect, id: room_id })
        ring_rooms[ring * config.sectors_per_ring + sector] = room_id
        room_id += 1
      }
    }
  }
  // Phase 2: Add spokes (radial corridors connecting rings)
  for sector in 0..<config.sectors_per_ring {
    if not(rng.rand_bool(config.spoke_chance)) {
      continue
    }
    for ring in 0..<(config.num_rings - 1) {
      let r1_id = ring_rooms[ring * config.sectors_per_ring + sector]
      let r2_id = ring_rooms[(ring + 1) * config.sectors_per_ring + sector]
      if r1_id >= 0 && r2_id >= 0 {
        let c1 = rooms[r1_id].rect.center()
        let c2 = rooms[r2_id].rect.center()
        grid.carve_l_corridor(c1, c2, @types.Corridor)
        edges.push({ from: r1_id, to: r2_id })
      }
    }
  }
  // Phase 3: Connect adjacent sectors within same ring
  for ring in 0..<config.num_rings {
    for sector in 0..<config.sectors_per_ring {
      let next_sector = (sector + 1) % config.sectors_per_ring
      let r1_id = ring_rooms[ring * config.sectors_per_ring + sector]
      let r2_id = ring_rooms[ring * config.sectors_per_ring + next_sector]
      if r1_id >= 0 && r2_id >= 0 {
        edges.push({ from: r1_id, to: r2_id })
      }
    }
  }
  // Phase 4: Ensure full connectivity via BFS on room graph
  // If disconnected, add corridors
  if rooms.length() > 1 {
    let visited = Array::make(rooms.length(), false)
    let queue : Array[Int] = [0]
    visited[0] = true
    while queue.length() > 0 {
      let cur = queue.unsafe_pop()
      for e in edges {
        if e.from == cur && not(visited[e.to]) {
          visited[e.to] = true
          queue.push(e.to)
        } else if e.to == cur && not(visited[e.from]) {
          visited[e.from] = true
          queue.push(e.from)
        }
      }
    }
    // Connect any unvisited rooms to nearest visited room
    for i in 0..<rooms.length() {
      if not(visited[i]) {
        let mut best_j = 0
        let mut best_dist = 99999
        let ci = rooms[i].rect.center()
        for j in 0..<rooms.length() {
          if visited[j] {
            let cj = rooms[j].rect.center()
            let dist = abs(ci.x - cj.x) + abs(ci.y - cj.y)
            if dist < best_dist {
              best_dist = dist
              best_j = j
            }
          }
        }
        let cj = rooms[best_j].rect.center()
        grid.carve_l_corridor(ci, cj, @types.Corridor)
        edges.push({ from: i, to: best_j })
        visited[i] = true
      }
    }
  }
  // Place Start at center and connect to innermost ring
  grid.set(cx, cy, @types.Start)
  // Carve corridor from center to nearest ring-0 room
  for sector in 0..<config.sectors_per_ring {
    let rid = ring_rooms[sector]
    if rid >= 0 {
      let c = rooms[rid].rect.center()
      grid.carve_l_corridor({ x: cx, y: cy }, c, @types.Corridor)
      break
    }
  }
  // Find room in outermost ring
  let mut goal_placed = false
  for sector in 0..<config.sectors_per_ring {
    let rid = ring_rooms[(config.num_rings - 1) * config.sectors_per_ring + sector]
    if rid >= 0 && not(goal_placed) {
      let c = rooms[rid].rect.center()
      grid.set(c.x, c.y, @types.Goal)
      goal_placed = true
    }
  }
  if not(goal_placed) && rooms.length() > 1 {
    let c = rooms[rooms.length() - 1].rect.center()
    grid.set(c.x, c.y, @types.Goal)
  }
  { grid, rooms, edges }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn get_sector(dx : Int, dy : Int, num_sectors : Int) -> Int {
  // Map (dx, dy) to sector index using atan2 approximation
  // Normalize angle to [0, 1) range
  // Simple approach: use octant-based mapping for 8 sectors
  if dx == 0 && dy == 0 {
    return 0
  }
  // Compute angle in [0, num_sectors) via atan2 approximation
  let angle = atan2_approx(dy, dx) // returns [0, 65536)
  (angle * num_sectors / 65536).clamp(min=0, max=num_sectors - 1)
}

///|
fn atan2_approx(y : Int, x : Int) -> Int {
  // Returns angle in [0, 65536) representing [0, 2*pi)
  // Using octant-based integer atan2
  let ax = if x < 0 { -x } else { x }
  let ay = if y < 0 { -y } else { y }
  // Base angle in first octant
  let base = if ax + ay == 0 {
    0
  } else {
    ay * 8192 / (ax + ay) // [0, 8192) for first octant
  }
  // Map to correct octant
  let angle = if x >= 0 && y >= 0 {
    // Quadrant 1 (0 to 16384)
    if ax >= ay { base } else { 16384 - base }
  } else if x < 0 && y >= 0 {
    // Quadrant 2 (16384 to 32768)
    if ax <= ay { 16384 + base } else { 32768 - base }
  } else if x < 0 && y < 0 {
    // Quadrant 3 (32768 to 49152)
    if ax >= ay { 32768 + base } else { 49152 - base }
  } else {
    // Quadrant 4 (49152 to 65536)
    if ax <= ay { 49152 + base } else { 65536 - base }
  }
  angle % 65536
}
