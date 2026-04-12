///|
pub(all) struct SpaceColonizationConfig {
  width : Int
  height : Int
  num_attractors : Int
  kill_distance : Int
  influence_distance : Int
  step_size : Int
  corridor_width : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn SpaceColonizationConfig::default() -> SpaceColonizationConfig {
  {
    width: 50,
    height: 25,
    num_attractors: 80,
    kill_distance: 2,
    influence_distance: 15,
    step_size: 1,
    corridor_width: 1,
    room_chance: 0.15,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct SpaceColonizationResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : SpaceColonizationConfig,
  rng : @types.RNG
) -> SpaceColonizationResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Place random attractor points
  let attractors : Array[@types.Point] = []
  let alive = Array::make(config.num_attractors, true)
  for _ in 0..<config.num_attractors {
    let x = rng.rand_range(2, config.width - 2)
    let y = rng.rand_range(2, config.height - 2)
    attractors.push({ x, y })
  }
  // Start with a single node at center
  let nodes : Array[@types.Point] = [
    { x: config.width / 2, y: config.height / 2 },
  ]
  grid.set(config.width / 2, config.height / 2, @types.Corridor)
  // Iterate
  let max_iter = 200
  for _ in 0..<max_iter {
    // For each node, find direction from influencing attractors
    let directions : Array[(Int, Int)] = []
    let has_influence : Array[Bool] = []
    for node in nodes {
      let mut dx = 0
      let mut dy = 0
      let mut count = 0
      for j in 0..<attractors.length() {
        if not(alive[j]) {
          continue
        }
        let dist = abs(node.x - attractors[j].x) +
          abs(node.y - attractors[j].y)
        if dist <= config.influence_distance {
          let adx = attractors[j].x - node.x
          let ady = attractors[j].y - node.y
          if adx > 0 {
            dx = dx + 1
          } else if adx < 0 {
            dx = dx - 1
          }
          if ady > 0 {
            dy = dy + 1
          } else if ady < 0 {
            dy = dy - 1
          }
          count = count + 1
        }
      }
      directions.push((dx, dy))
      has_influence.push(count > 0)
    }
    // Grow new nodes
    let mut grew = false
    let new_nodes : Array[@types.Point] = []
    for i in 0..<nodes.length() {
      if not(has_influence[i]) {
        continue
      }
      let dir = directions[i]
      let mut step_x = 0
      let mut step_y = 0
      if abs(dir.0) >= abs(dir.1) {
        step_x = if dir.0 > 0 { config.step_size } else { -config.step_size }
      } else {
        step_y = if dir.1 > 0 { config.step_size } else { -config.step_size }
      }
      let nx = nodes[i].x + step_x
      let ny = nodes[i].y + step_y
      if nx > 0 &&
        nx < config.width - 1 &&
        ny > 0 &&
        ny < config.height - 1 &&
        grid.get(nx, ny) == @types.Wall {
        grid.set(nx, ny, @types.Corridor)
        // Also carve path from parent to new node
        carve_path(grid, nodes[i], { x: nx, y: ny })
        new_nodes.push({ x: nx, y: ny })
        grew = true
      }
    }
    for p in new_nodes {
      nodes.push(p)
    }
    // Kill attractors too close to any node
    for j in 0..<attractors.length() {
      if not(alive[j]) {
        continue
      }
      for node in nodes {
        let dist = abs(node.x - attractors[j].x) +
          abs(node.y - attractors[j].y)
        if dist <= config.kill_distance {
          alive[j] = false
          break
        }
      }
    }
    if not(grew) {
      break
    }
  }
  // Place rooms at random nodes
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for node in nodes {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = node.x - rw / 2
    let ry = node.y - rh / 2
    if rx > 0 && ry > 0 && rx + rw < config.width - 1 && ry + rh < config.height -
    1 {
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for r in rooms {
        if rect.intersects(r.rect) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, config)
  // Place Start at first node, Goal at farthest
  if nodes.length() > 0 {
    grid.set(nodes[0].x, nodes[0].y, @types.Start)
    let mut max_dist = 0
    let mut goal_idx = 0
    for i in 1..<nodes.length() {
      let dist = abs(nodes[i].x - nodes[0].x) + abs(nodes[i].y - nodes[0].y)
      if dist > max_dist {
        max_dist = dist
        goal_idx = i
      }
    }
    if goal_idx > 0 {
      grid.set(nodes[goal_idx].x, nodes[goal_idx].y, @types.Goal)
    }
  }
  { grid, rooms }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn carve_path(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if grid.in_bounds(x, y) && grid.get(x, y) == @types.Wall {
      grid.set(x, y, @types.Corridor)
    }
    if x != to.x {
      x = if x < to.x { x + 1 } else { x - 1 }
    } else {
      y = if y < to.y { y + 1 } else { y - 1 }
    }
  }
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : SpaceColonizationConfig
) -> Unit {
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      let t = grid.get(x, y)
      if not(visited[idx]) &&
        (t == @types.Floor || t == @types.Corridor || t == @types.Door) {
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
    grid.carve_l_corridor(a, b, @types.Corridor)
  }
}
