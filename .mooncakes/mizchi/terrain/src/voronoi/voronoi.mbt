///|
pub(all) struct VoronoiConfig {
  width : Int
  height : Int
  num_seeds : Int
  shrink : Int
  extra_edge_chance : Double
}

///|
pub fn VoronoiConfig::default() -> VoronoiConfig {
  { width: 40, height: 20, num_seeds: 8, shrink: 1, extra_edge_chance: 0.2 }
}

///|
pub(all) struct VoronoiResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
pub fn generate(config : VoronoiConfig, rng : @types.RNG) -> VoronoiResult {
  // Phase 1: Place seed points with minimum distance check
  let seeds = place_seeds(config, rng)
  let n = seeds.length()
  // Phase 2: Assign each cell to nearest seed (Manhattan distance)
  let region_map = Array::make(config.width * config.height, -1)
  for y in 0..<config.height {
    for x in 0..<config.width {
      let mut best_dist = config.width + config.height
      let mut best_id = 0
      for i in 0..<n {
        let dist = abs(x - seeds[i].x) + abs(y - seeds[i].y)
        if dist < best_dist {
          best_dist = dist
          best_id = i
        }
      }
      region_map[y * config.width + x] = best_id
    }
  }
  // Phase 3: Build grid - mark boundary cells as Wall (shrink)
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  for y in 0..<config.height {
    for x in 0..<config.width {
      // Border of the map is always wall
      if x == 0 || y == 0 || x == config.width - 1 || y == config.height - 1 {
        continue
      }
      let my_region = region_map[y * config.width + x]
      // Check if near boundary (within shrink distance)
      let mut is_boundary = false
      for dy in -config.shrink..=config.shrink {
        for dx in -config.shrink..=config.shrink {
          let nx = x + dx
          let ny = y + dy
          if nx >= 0 &&
            nx < config.width &&
            ny >= 0 &&
            ny < config.height &&
            region_map[ny * config.width + nx] != my_region {
            is_boundary = true
          }
        }
      }
      if not(is_boundary) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Phase 4: Compute bounding rects for each region as rooms
  let rooms = compute_rooms(grid, region_map, seeds, config)
  // Phase 5: Find adjacent region pairs
  let adj = find_adjacent_pairs(region_map, config.width, config.height, n)
  // Phase 6: Prim's MST on seeds using Manhattan distance
  let mst_edges = prims_mst(seeds, adj, n)
  let edges : Array[@types.Edge] = []
  for e in mst_edges {
    edges.push(e)
  }
  // Phase 7: Add extra edges
  for pair in adj {
    let a = pair.0
    let b = pair.1
    let mut in_mst = false
    for e in mst_edges {
      if (e.from == a && e.to == b) || (e.from == b && e.to == a) {
        in_mst = true
        break
      }
    }
    if not(in_mst) && rng.rand_bool(config.extra_edge_chance) {
      edges.push({ from: a, to: b })
    }
  }
  // Phase 8: Carve corridors for all edges
  for edge in edges {
    let from = seeds[edge.from]
    let to = seeds[edge.to]
    grid.carve_l_corridor(from, to, @types.Corridor)
  }
  // Phase 9: Place Start at seed[0], Goal at farthest seed
  let mut max_dist = 0
  let mut goal_id = 0
  for i in 1..<n {
    let dist = abs(seeds[i].x - seeds[0].x) + abs(seeds[i].y - seeds[0].y)
    if dist > max_dist {
      max_dist = dist
      goal_id = i
    }
  }
  grid.set(seeds[0].x, seeds[0].y, @types.Start)
  grid.set(seeds[goal_id].x, seeds[goal_id].y, @types.Goal)
  { grid, rooms, edges }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn place_seeds(
  config : VoronoiConfig,
  rng : @types.RNG
) -> Array[@types.Point] {
  let seeds : Array[@types.Point] = []
  let min_dist = ((config.width + config.height) / config.num_seeds).clamp(
    min=3,
    max=10,
  )
  let max_attempts = 100
  for _ in 0..<config.num_seeds {
    let mut placed = false
    for _ in 0..<max_attempts {
      let x = rng.rand_range(2, config.width - 2)
      let y = rng.rand_range(2, config.height - 2)
      let mut too_close = false
      for s in seeds {
        if abs(x - s.x) + abs(y - s.y) < min_dist {
          too_close = true
          break
        }
      }
      if not(too_close) {
        seeds.push({ x, y })
        placed = true
        break
      }
    }
    if not(placed) {
      // Fallback: place anywhere
      seeds.push({
        x: rng.rand_range(2, config.width - 2),
        y: rng.rand_range(2, config.height - 2),
      })
    }
  }
  seeds
}

///|
fn compute_rooms(
  grid : @types.Grid2D,
  region_map : Array[Int],
  seeds : Array[@types.Point],
  config : VoronoiConfig
) -> Array[@types.Room] {
  let n = seeds.length()
  let rooms : Array[@types.Room] = []
  for i in 0..<n {
    let mut min_x = config.width
    let mut min_y = config.height
    let mut max_x = 0
    let mut max_y = 0
    let mut has_floor = false
    for y in 0..<config.height {
      for x in 0..<config.width {
        if region_map[y * config.width + x] == i &&
          grid.get(x, y) == @types.Floor {
          if x < min_x {
            min_x = x
          }
          if y < min_y {
            min_y = y
          }
          if x > max_x {
            max_x = x
          }
          if y > max_y {
            max_y = y
          }
          has_floor = true
        }
      }
    }
    if has_floor {
      rooms.push({
        rect: { x: min_x, y: min_y, w: max_x - min_x + 1, h: max_y - min_y + 1 },
        id: i,
      })
    }
  }
  rooms
}

///|
fn find_adjacent_pairs(
  region_map : Array[Int],
  width : Int,
  height : Int,
  n : Int
) -> Array[(Int, Int)] {
  // Use a flat boolean array to track seen pairs
  let seen = Array::make(n * n, false)
  let pairs : Array[(Int, Int)] = []
  for y in 0..<height {
    for x in 0..<width {
      let r = region_map[y * width + x]
      if r < 0 {
        continue
      }
      // Check right
      if x + 1 < width {
        let r2 = region_map[y * width + (x + 1)]
        if r2 >= 0 && r2 != r {
          let a = r.min(r2)
          let b = r.max(r2)
          if not(seen[a * n + b]) {
            seen[a * n + b] = true
            pairs.push((a, b))
          }
        }
      }
      // Check down
      if y + 1 < height {
        let r2 = region_map[(y + 1) * width + x]
        if r2 >= 0 && r2 != r {
          let a = r.min(r2)
          let b = r.max(r2)
          if not(seen[a * n + b]) {
            seen[a * n + b] = true
            pairs.push((a, b))
          }
        }
      }
    }
  }
  pairs
}

///|
fn prims_mst(
  seeds : Array[@types.Point],
  adj : Array[(Int, Int)],
  n : Int
) -> Array[@types.Edge] {
  if n <= 1 {
    return []
  }
  let in_tree = Array::make(n, false)
  in_tree[0] = true
  let edges : Array[@types.Edge] = []
  let mut tree_size = 1
  while tree_size < n {
    let mut best_dist = 999999
    let mut best_from = 0
    let mut best_to = 0
    let mut found = false
    for pair in adj {
      let a = pair.0
      let b = pair.1
      if in_tree[a] && not(in_tree[b]) {
        let dist = abs(seeds[a].x - seeds[b].x) + abs(seeds[a].y - seeds[b].y)
        if dist < best_dist {
          best_dist = dist
          best_from = a
          best_to = b
          found = true
        }
      } else if in_tree[b] && not(in_tree[a]) {
        let dist = abs(seeds[a].x - seeds[b].x) + abs(seeds[a].y - seeds[b].y)
        if dist < best_dist {
          best_dist = dist
          best_from = b
          best_to = a
          found = true
        }
      }
    }
    if found {
      in_tree[best_to] = true
      edges.push({ from: best_from, to: best_to })
      tree_size += 1
    } else {
      // No more reachable nodes via adjacency; connect nearest unreached node
      let mut min_dist = 999999
      let mut min_from = 0
      let mut min_to = 0
      for i in 0..<n {
        if in_tree[i] {
          for j in 0..<n {
            if not(in_tree[j]) {
              let dist = abs(seeds[i].x - seeds[j].x) +
                abs(seeds[i].y - seeds[j].y)
              if dist < min_dist {
                min_dist = dist
                min_from = i
                min_to = j
              }
            }
          }
        }
      }
      in_tree[min_to] = true
      edges.push({ from: min_from, to: min_to })
      tree_size += 1
    }
  }
  edges
}
