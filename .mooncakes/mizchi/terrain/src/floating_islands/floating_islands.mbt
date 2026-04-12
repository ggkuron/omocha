///|
pub(all) struct FloatingIslandsConfig {
  width : Int
  height : Int
  max_height : Int
  num_islands : Int
  bridge_chance : Double
}

///|
pub fn FloatingIslandsConfig::default() -> FloatingIslandsConfig {
  { width: 48, height: 48, max_height: 8, num_islands: 6, bridge_chance: 0.6 }
}

///|
pub(all) struct FloatingIslandsResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : FloatingIslandsConfig,
  rng : @types.RNG
) -> FloatingIslandsResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  // Island data: center, radius, height
  let centers : Array[@types.Point] = []
  let radii : Array[Int] = []
  let island_heights : Array[Int] = []
  // Place islands
  let min_radius = 3
  let max_radius = 7
  let mut room_id = 0
  for _ in 0..<config.num_islands {
    let mut placed = false
    let mut attempts = 0
    while not(placed) && attempts < 200 {
      attempts += 1
      let r = rng.rand_range(min_radius, max_radius + 1)
      let cx = rng.rand_range(r + 1, w - r - 1)
      let cy = rng.rand_range(r + 1, h - r - 1)
      // Check no overlap with existing islands (with gap)
      let mut overlaps = false
      for i in 0..<centers.length() {
        let dx = cx - centers[i].x
        let dy = cy - centers[i].y
        let dist = isqrt(dx * dx + dy * dy)
        if dist < r + radii[i] + 2 {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        let island_h = rng.rand_range(2, config.max_height + 1)
        centers.push({ x: cx, y: cy })
        radii.push(r)
        island_heights.push(island_h)
        // Carve roughly circular island
        let mut min_x = w
        let mut min_y = h
        let mut max_x = 0
        let mut max_y = 0
        for dy in -r..=r {
          for dx in -r..=r {
            let dist_sq = dx * dx + dy * dy
            let r_sq = r * r
            // Add noise for irregular shape
            let noise = rng.rand_range(-1, 2)
            let threshold = r_sq + noise * r
            if dist_sq <= threshold {
              let px = cx + dx
              let py = cy + dy
              if px >= 0 && px < w && py >= 0 && py < h {
                // Edge of island is slightly lower
                let edge_dist = isqrt(dist_sq)
                let edge_factor = (r - edge_dist).to_double() /
                  r.to_double()
                let cell_h = (island_h.to_double() * edge_factor)
                  .to_int()
                  .max(1)
                grid.set(px, py, @types.Floor)
                grid.set_height(px, py, cell_h)
                if px < min_x {
                  min_x = px
                }
                if py < min_y {
                  min_y = py
                }
                if px > max_x {
                  max_x = px
                }
                if py > max_y {
                  max_y = py
                }
              }
            }
          }
        }
        // Add wall border around the island (cliff edge)
        add_island_border(grid, cx, cy, r, island_h, w, h)
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
        placed = true
      }
    }
  }
  // Connect islands with bridges using MST + extra random bridges
  if centers.length() > 1 {
    let n = centers.length()
    // Build MST using Prim's algorithm
    let in_mst = Array::make(n, false)
    let min_dist = Array::make(n, 999999)
    let min_edge = Array::make(n, 0)
    in_mst[0] = true
    for i in 1..<n {
      let d = dist_sq(centers[0], centers[i])
      min_dist[i] = d
      min_edge[i] = 0
    }
    let mst_edges : Array[(Int, Int)] = []
    for _ in 1..<n {
      let mut best = -1
      let mut best_d = 999999
      for j in 0..<n {
        if not(in_mst[j]) && min_dist[j] < best_d {
          best_d = min_dist[j]
          best = j
        }
      }
      if best < 0 {
        break
      }
      in_mst[best] = true
      mst_edges.push((min_edge[best], best))
      for j in 0..<n {
        if not(in_mst[j]) {
          let d = dist_sq(centers[best], centers[j])
          if d < min_dist[j] {
            min_dist[j] = d
            min_edge[j] = best
          }
        }
      }
    }
    // Build MST bridges
    for edge in mst_edges {
      let from_idx = edge.0
      let to_idx = edge.1
      let from_h = island_heights[from_idx]
      let to_h = island_heights[to_idx]
      build_bridge(grid, centers[from_idx], centers[to_idx], from_h, to_h, w, h)
    }
    // Add extra random bridges
    for i in 0..<n {
      for j in (i + 1)..<n {
        // Skip MST edges (already connected)
        let mut is_mst = false
        for e in mst_edges {
          if (e.0 == i && e.1 == j) || (e.0 == j && e.1 == i) {
            is_mst = true
            break
          }
        }
        if not(is_mst) && rng.rand_bool(config.bridge_chance) {
          let from_h = island_heights[i]
          let to_h = island_heights[j]
          build_bridge(grid, centers[i], centers[j], from_h, to_h, w, h)
        }
      }
    }
  }
  // Place Start on first island, Goal on last island
  if centers.length() > 0 {
    let start_p = centers[0]
    grid.set(start_p.x, start_p.y, @types.Start)
    let goal_p = centers[centers.length() - 1]
    if goal_p.x != start_p.x || goal_p.y != start_p.y {
      grid.set(goal_p.x, goal_p.y, @types.Goal)
    } else {
      // Fallback: find another floor cell
      let mut goal_placed = false
      for y = h - 2; y > 0; y = y - 1 {
        for x = w - 2; x > 0; x = x - 1 {
          if not(goal_placed) && grid.get(x, y) == @types.Floor {
            grid.set(x, y, @types.Goal)
            goal_placed = true
          }
        }
      }
    }
  }
  { grid, rooms }
}

///|
fn add_island_border(
  grid : @types.HeightGrid,
  cx : Int,
  cy : Int,
  radius : Int,
  island_h : Int,
  w : Int,
  h : Int
) -> Unit {
  let r_outer = radius + 1
  let dirs : Array[(Int, Int)] = [
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
  ]
  for dy in -r_outer..=r_outer {
    for dx in -r_outer..=r_outer {
      let px = cx + dx
      let py = cy + dy
      if px >= 0 && px < w && py >= 0 && py < h && grid.get(px, py) == @types.Empty {
        // Check if adjacent to a floor cell
        let mut adjacent_to_floor = false
        for d in dirs {
          let nx = px + d.0
          let ny = py + d.1
          if nx >= 0 && nx < w && ny >= 0 && ny < h &&
            grid.get(nx, ny) == @types.Floor {
            adjacent_to_floor = true
            break
          }
        }
        if adjacent_to_floor {
          grid.set(px, py, @types.Wall)
          grid.set_height(px, py, island_h)
        }
      }
    }
  }
}

///|
fn build_bridge(
  grid : @types.HeightGrid,
  from : @types.Point,
  to : @types.Point,
  from_h : Int,
  to_h : Int,
  w : Int,
  h : Int
) -> Unit {
  // Draw an L-shaped bridge between two islands
  // First horizontal, then vertical
  let mut x = from.x
  let mut y = from.y
  let total_steps = abs(to.x - from.x) + abs(to.y - from.y)
  let mut step = 0
  // Horizontal segment
  while x != to.x {
    if x >= 0 && x < w && y >= 0 && y < h {
      let t = if total_steps > 0 {
        step.to_double() / total_steps.to_double()
      } else {
        0.0
      }
      let bridge_h = from_h + ((to_h - from_h).to_double() * t).to_int()
      let cur_tile = grid.get(x, y)
      if cur_tile == @types.Empty {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, bridge_h)
      }
    }
    if x < to.x {
      x += 1
    } else {
      x -= 1
    }
    step += 1
  }
  // Vertical segment
  while y != to.y {
    if x >= 0 && x < w && y >= 0 && y < h {
      let t = if total_steps > 0 {
        step.to_double() / total_steps.to_double()
      } else {
        1.0
      }
      let bridge_h = from_h + ((to_h - from_h).to_double() * t).to_int()
      let cur_tile = grid.get(x, y)
      if cur_tile == @types.Empty {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, bridge_h)
      }
    }
    if y < to.y {
      y += 1
    } else {
      y -= 1
    }
    step += 1
  }
  // Final cell
  if x >= 0 && x < w && y >= 0 && y < h {
    let cur_tile = grid.get(x, y)
    if cur_tile == @types.Empty {
      grid.set(x, y, @types.Corridor)
      grid.set_height(x, y, to_h)
    }
  }
}

///|
fn dist_sq(a : @types.Point, b : @types.Point) -> Int {
  let dx = a.x - b.x
  let dy = a.y - b.y
  dx * dx + dy * dy
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
