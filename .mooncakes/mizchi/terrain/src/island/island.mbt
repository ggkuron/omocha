///|
pub(all) struct IslandConfig {
  width : Int
  height : Int
  num_islands : Int
  island_min_radius : Int
  island_max_radius : Int
  bridge_width : Int
  fill_ratio : Double
}

///|
pub fn IslandConfig::default() -> IslandConfig {
  {
    width: 50,
    height: 25,
    num_islands: 5,
    island_min_radius: 3,
    island_max_radius: 7,
    bridge_width: 1,
    fill_ratio: 0.4,
  }
}

///|
pub(all) struct IslandResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : IslandConfig, rng : @types.RNG) -> IslandResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Water)
  // Place islands as circular blobs
  let centers : Array[@types.Point] = []
  let radii : Array[Int] = []
  for _ in 0..<config.num_islands {
    // Pick a random center with margin
    let max_r = config.island_max_radius
    let cx = rng.rand_range(max_r + 1, w - max_r - 1)
    let cy = rng.rand_range(max_r + 1, h - max_r - 1)
    let r = rng.rand_range(config.island_min_radius, config.island_max_radius + 1)
    centers.push({ x: cx, y: cy })
    radii.push(r)
    // Carve island: for each cell within radius (+ noise), set to Floor
    let r_sq = r * r
    for dy in -r..=r {
      for dx in -r..=r {
        let dist_sq = dx * dx + dy * dy
        // Add noise: allow some cells slightly beyond radius, reject some within
        let noise = rng.rand_range(-2, 3)
        let threshold_sq = r_sq + noise * r
        if dist_sq <= threshold_sq {
          let px = cx + dx
          let py = cy + dy
          if px >= 1 && px < w - 1 && py >= 1 && py < h - 1 {
            grid.set(px, py, @types.Floor)
          }
        }
      }
    }
  }
  // Run cellular automata smoothing on the islands
  for _ in 0..<3 {
    let old = grid.clone()
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let floor_count = count_tile_neighbors(old, x, y, @types.Floor)
        if old.get(x, y) == @types.Floor {
          // Kill isolated floor cells
          if floor_count < 2 {
            grid.set(x, y, @types.Water)
          }
        } else if old.get(x, y) == @types.Water {
          // Birth: surrounded by many floor cells
          if floor_count >= 5 {
            grid.set(x, y, @types.Floor)
          }
        }
      }
    }
  }
  // Build MST of island centers using Prim's algorithm
  if centers.length() > 1 {
    let n = centers.length()
    let in_mst = Array::make(n, false)
    // Store min distance to MST for each node
    let min_dist = Array::make(n, 999999)
    let min_edge = Array::make(n, 0) // which MST node connects to
    in_mst[0] = true
    // Initialize distances from node 0
    for i in 1..<n {
      let d = dist_sq(centers[0], centers[i])
      min_dist[i] = d
      min_edge[i] = 0
    }
    let mst_edges : Array[(Int, Int)] = []
    for _ in 1..<n {
      // Find closest non-MST node
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
      // Update distances
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
    // Carve bridges along MST edges
    for edge in mst_edges {
      let from = centers[edge.0]
      let to = centers[edge.1]
      carve_bridge(grid, from, to, config.bridge_width, w, h)
    }
  }
  // Place Start on first island center, Goal on last island center
  if centers.length() > 0 {
    let start = find_nearest_tile(grid, centers[0].x, centers[0].y, @types.Floor)
    grid.set(start.x, start.y, @types.Start)
    let last = centers[centers.length() - 1]
    let goal = find_nearest_tile(grid, last.x, last.y, @types.Floor)
    // Make sure goal is not same as start
    if goal.x == start.x && goal.y == start.y {
      // Find any other floor tile
      let alt = find_alt_floor(grid, w, h, start)
      grid.set(alt.x, alt.y, @types.Goal)
    } else {
      grid.set(goal.x, goal.y, @types.Goal)
    }
  }
  // Build rooms from islands
  let rooms = find_rooms(grid, w, h)
  { grid, rooms }
}

///|
fn count_tile_neighbors(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  tile : @types.Tile
) -> Int {
  let mut count = 0
  for dy in -1..=1 {
    for dx in -1..=1 {
      if dx == 0 && dy == 0 {
        continue
      }
      let nx = x + dx
      let ny = y + dy
      if grid.in_bounds(nx, ny) && grid.get(nx, ny) == tile {
        count += 1
      }
    }
  }
  count
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
fn carve_bridge(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  bridge_width : Int,
  w : Int,
  h : Int
) -> Unit {
  // Walk from `from` to `to` using L-shaped path
  // For each step, carve a corridor of bridge_width
  let half_w = bridge_width / 2
  // Horizontal segment first, then vertical
  let mut x = from.x
  let mut y = from.y
  // Move horizontally towards to.x
  while x != to.x {
    for dy in -half_w..=half_w {
      let py = y + dy
      if px_in_bounds(x, py, w, h) {
        let current = grid.get(x, py)
        if current == @types.Water {
          grid.set(x, py, @types.Corridor)
        } else if current == @types.Wall {
          grid.set(x, py, @types.Corridor)
        }
      }
    }
    if x < to.x {
      x += 1
    } else {
      x -= 1
    }
  }
  // Move vertically towards to.y
  while y != to.y {
    for dx in -half_w..=half_w {
      let px = x + dx
      if px_in_bounds(px, y, w, h) {
        let current = grid.get(px, y)
        if current == @types.Water {
          grid.set(px, y, @types.Corridor)
        } else if current == @types.Wall {
          grid.set(px, y, @types.Corridor)
        }
      }
    }
    if y < to.y {
      y += 1
    } else {
      y -= 1
    }
  }
  // Set the final cell
  for dx in -half_w..=half_w {
    for dy in -half_w..=half_w {
      let px = x + dx
      let py = y + dy
      if px_in_bounds(px, py, w, h) {
        let current = grid.get(px, py)
        if current == @types.Water {
          grid.set(px, py, @types.Corridor)
        } else if current == @types.Wall {
          grid.set(px, py, @types.Corridor)
        }
      }
    }
  }
}

///|
fn px_in_bounds(x : Int, y : Int, w : Int, h : Int) -> Bool {
  x >= 1 && x < w - 1 && y >= 1 && y < h - 1
}

///|
fn find_nearest_tile(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int,
  tile : @types.Tile
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        if abs(dx) + abs(dy) != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && grid.get(x, y) == tile {
          return { x, y }
        }
      }
    }
  }
  { x: cx, y: cy }
}

///|
fn find_alt_floor(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  exclude : @types.Point
) -> @types.Point {
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if grid.get(x, y) == @types.Floor && (x != exclude.x || y != exclude.y) {
        return { x, y }
      }
    }
  }
  exclude
}

///|
fn find_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int
) -> Array[@types.Room] {
  let visited = Array::make(w * h, false)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let idx = y * w + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        let comp = flood_fill_floor(grid, x, y, w, h)
        let mut min_x = w
        let mut min_y = h
        let mut max_x = 0
        let mut max_y = 0
        for p in comp {
          visited[p.y * w + p.x] = true
          if p.x < min_x {
            min_x = p.x
          }
          if p.y < min_y {
            min_y = p.y
          }
          if p.x > max_x {
            max_x = p.x
          }
          if p.y > max_y {
            max_y = p.y
          }
        }
        if comp.length() > 4 {
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
      }
    }
  }
  rooms
}

///|
fn flood_fill_floor(
  grid : @types.Grid2D,
  start_x : Int,
  start_y : Int,
  w : Int,
  h : Int
) -> Array[@types.Point] {
  ignore(h)
  let visited : Array[@types.Point] = []
  let seen = Array::make(w * grid.height, false)
  let stack : Array[@types.Point] = [{ x: start_x, y: start_y }]
  seen[start_y * w + start_x] = true
  while stack.length() > 0 {
    let p = stack.pop().unwrap()
    visited.push(p)
    let neighbors = grid.neighbors4(p.x, p.y)
    for n in neighbors {
      let idx = n.y * w + n.x
      if not(seen[idx]) && grid.get(n.x, n.y) == @types.Floor {
        seen[idx] = true
        stack.push(n)
      }
    }
  }
  visited
}
