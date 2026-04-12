///|
pub(all) struct BfsRoomsConfig {
  width : Int
  height : Int
  num_seeds : Int
  min_room_size : Int
  parallel_steps : Int
  serial_steps : Int
  door_chance : Double
  extra_door_chance : Double
}

///|
pub fn BfsRoomsConfig::default() -> BfsRoomsConfig {
  {
    width: 50,
    height: 25,
    num_seeds: 10,
    min_room_size: 8,
    parallel_steps: 12,
    serial_steps: 20,
    door_chance: 0.8,
    extra_door_chance: 0.15,
  }
}

///|
pub(all) struct BfsRoomsResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
pub fn generate(config : BfsRoomsConfig, rng : @types.RNG) -> BfsRoomsResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Owner map: which room owns each cell (-1 = none)
  let owner = Array::make(config.width * config.height, -1)
  // Phase 1: Place seed points with minimum distance
  let seeds = place_seeds(config, rng)
  let n = seeds.length()
  // Initialize frontiers for each seed
  let frontiers : Array[Array[@types.Point]] = []
  for i in 0..<n {
    let s = seeds[i]
    frontiers.push([s])
    owner[s.y * config.width + s.x] = i
  }
  // Phase 2: Parallel BFS expansion (all rooms grow simultaneously)
  for _ in 0..<config.parallel_steps {
    for i in 0..<n {
      expand_frontier(grid, owner, frontiers[i], i, config)
    }
  }
  // Phase 3: Serial BFS expansion (one room at a time, fills remaining space)
  for _ in 0..<config.serial_steps {
    for i in 0..<n {
      expand_frontier(grid, owner, frontiers[i], i, config)
    }
  }
  // Carve floors for owned cells
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if owner[y * config.width + x] >= 0 {
        // Check if this is a boundary cell (adjacent to different owner)
        let my_owner = owner[y * config.width + x]
        let mut is_boundary = false
        let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) {
            let other = owner[ny * config.width + nx]
            if other >= 0 && other != my_owner {
              is_boundary = true
            }
          }
        }
        if is_boundary {
          // Keep as wall (room boundary)
          grid.set(x, y, @types.Wall)
        } else {
          grid.set(x, y, @types.Floor)
        }
      }
    }
  }
  // Phase 4: Compute bounding rects for rooms
  let rooms = compute_rooms(grid, owner, n, config)
  // Phase 5: Find adjacent room pairs and place doors
  let adj = find_adjacent_pairs(owner, config.width, config.height, n)
  let edges : Array[@types.Edge] = []
  // MST for guaranteed connectivity
  let mst = build_mst(seeds, adj, n)
  for e in mst {
    edges.push(e)
  }
  // Extra doors
  for pair in adj {
    let a = pair.0
    let b = pair.1
    let mut in_mst = false
    for e in mst {
      if (e.from == a && e.to == b) || (e.from == b && e.to == a) {
        in_mst = true
        break
      }
    }
    if not(in_mst) && rng.rand_bool(config.extra_door_chance) {
      edges.push({ from: a, to: b })
    }
  }
  // Place doors for each edge
  for edge in edges {
    place_door_between(grid, owner, edge.from, edge.to, config, rng)
  }
  // Ensure grid-level connectivity
  ensure_connectivity(grid, config)
  // Place Start and Goal
  if rooms.length() > 0 {
    let sc = seeds[0]
    grid.set(sc.x, sc.y, @types.Start)
  }
  if rooms.length() > 1 {
    // Farthest seed from seed[0]
    let mut max_dist = 0
    let mut goal_id = 0
    for i in 1..<n {
      let dist = abs(seeds[i].x - seeds[0].x) + abs(seeds[i].y - seeds[0].y)
      if dist > max_dist {
        max_dist = dist
        goal_id = i
      }
    }
    grid.set(seeds[goal_id].x, seeds[goal_id].y, @types.Goal)
  }
  { grid, rooms, edges }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn place_seeds(
  config : BfsRoomsConfig,
  rng : @types.RNG
) -> Array[@types.Point] {
  let seeds : Array[@types.Point] = []
  let min_dist = ((config.width + config.height) / config.num_seeds).clamp(
    min=4,
    max=12,
  )
  for _ in 0..<config.num_seeds {
    for _ in 0..<100 {
      let x = rng.rand_range(3, config.width - 3)
      let y = rng.rand_range(3, config.height - 3)
      let mut too_close = false
      for s in seeds {
        if abs(x - s.x) + abs(y - s.y) < min_dist {
          too_close = true
          break
        }
      }
      if not(too_close) {
        seeds.push({ x, y })
        break
      }
    }
  }
  seeds
}

///|
fn expand_frontier(
  _grid : @types.Grid2D,
  owner : Array[Int],
  frontier : Array[@types.Point],
  room_id : Int,
  config : BfsRoomsConfig
) -> Unit {
  let next : Array[@types.Point] = []
  for p in frontier {
    let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for d in dirs {
      let nx = p.x + d.0
      let ny = p.y + d.1
      if nx > 0 &&
        nx < config.width - 1 &&
        ny > 0 &&
        ny < config.height - 1 {
        let idx = ny * config.width + nx
        if owner[idx] < 0 {
          owner[idx] = room_id
          next.push({ x: nx, y: ny })
        }
      }
    }
  }
  frontier.clear()
  for p in next {
    frontier.push(p)
  }
}

///|
fn compute_rooms(
  grid : @types.Grid2D,
  owner : Array[Int],
  n : Int,
  config : BfsRoomsConfig
) -> Array[@types.Room] {
  ignore(owner)
  ignore(n)
  let rooms : Array[@types.Room] = []
  // Use flood fill to find contiguous floor regions
  let visited = Array::make(config.width * config.height, false)
  let mut room_id = 0
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        let comp = @types.flood_fill(grid, x, y)
        let mut min_x = config.width
        let mut min_y = config.height
        let mut max_x = 0
        let mut max_y = 0
        for p in comp {
          visited[p.y * config.width + p.x] = true
          if p.x < min_x { min_x = p.x }
          if p.y < min_y { min_y = p.y }
          if p.x > max_x { max_x = p.x }
          if p.y > max_y { max_y = p.y }
        }
        rooms.push({
          rect: {
            x: min_x,
            y: min_y,
            w: max_x - min_x + 1,
            h: max_y - min_y + 1,
          },
          id: room_id,
        })
        room_id += 1
      }
    }
  }
  rooms
}

///|
fn find_adjacent_pairs(
  owner : Array[Int],
  width : Int,
  height : Int,
  n : Int
) -> Array[(Int, Int)] {
  let seen = Array::make(n * n, false)
  let pairs : Array[(Int, Int)] = []
  for y in 0..<height {
    for x in 0..<width {
      let r = owner[y * width + x]
      if r < 0 {
        continue
      }
      if x + 1 < width {
        let r2 = owner[y * width + (x + 1)]
        if r2 >= 0 && r2 != r {
          let a = r.min(r2)
          let b = r.max(r2)
          if not(seen[a * n + b]) {
            seen[a * n + b] = true
            pairs.push((a, b))
          }
        }
      }
      if y + 1 < height {
        let r2 = owner[(y + 1) * width + x]
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
fn build_mst(
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
      // Connect nearest unreached seed
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

///|
fn place_door_between(
  grid : @types.Grid2D,
  owner : Array[Int],
  a : Int,
  b : Int,
  config : BfsRoomsConfig,
  rng : @types.RNG
) -> Unit {
  // Find boundary wall cells between regions a and b
  let candidates : Array[@types.Point] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if grid.get(x, y) != @types.Wall {
        continue
      }
      let idx = y * config.width + x
      let cell_owner = owner[idx]
      if cell_owner != a && cell_owner != b {
        continue
      }
      let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      let mut touches_a = cell_owner == a
      let mut touches_b = cell_owner == b
      for d in dirs {
        let nx = x + d.0
        let ny = y + d.1
        if grid.in_bounds(nx, ny) {
          let no = owner[ny * config.width + nx]
          if no == a { touches_a = true }
          if no == b { touches_b = true }
        }
      }
      if touches_a && touches_b {
        candidates.push({ x, y })
      }
    }
  }
  if candidates.length() > 0 {
    let door = rng.rand_pick(candidates)
    grid.set(door.x, door.y, @types.Door)
  }
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : BfsRoomsConfig
) -> Unit {
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      let t = grid.get(x, y)
      if not(visited[idx]) &&
        (t == @types.Floor || t == @types.Door || t == @types.Start || t == @types.Goal) {
        let comp = @types.flood_fill(grid, x, y)
        for p in comp {
          visited[p.y * config.width + p.x] = true
        }
        components.push(comp)
      }
    }
  }
  if components.length() <= 1 {
    return
  }
  let mut largest_idx = 0
  let mut largest_size = 0
  for i in 0..<components.length() {
    if components[i].length() > largest_size {
      largest_size = components[i].length()
      largest_idx = i
    }
  }
  for i in 0..<components.length() {
    if i == largest_idx {
      continue
    }
    let a = components[i][0]
    let b = components[largest_idx][0]
    grid.carve_l_corridor(a, b, @types.Door)
  }
}
