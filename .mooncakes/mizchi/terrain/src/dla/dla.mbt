///|
pub(all) struct DlaConfig {
  width : Int
  height : Int
  target_fill : Double
  max_walk_steps : Int
  num_seeds : Int
}

///|
pub fn DlaConfig::default() -> DlaConfig {
  {
    width: 40,
    height: 20,
    target_fill: 0.35,
    max_walk_steps: 500,
    num_seeds: 3,
  }
}

///|
pub(all) struct DlaResult {
  grid : @types.Grid2D
  particles : Int
}

///|
pub fn generate(config : DlaConfig, rng : @types.RNG) -> DlaResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let inner_w = config.width - 2
  let inner_h = config.height - 2
  let target = (inner_w.to_double() * inner_h.to_double() * config.target_fill).to_int()
  let mut carved = 0
  // Place initial seeds
  for _ in 0..<config.num_seeds {
    let x = rng.rand_range(2, config.width - 2)
    let y = rng.rand_range(2, config.height - 2)
    if grid.get(x, y) == @types.Wall {
      grid.set(x, y, @types.Floor)
      carved += 1
    }
  }
  // Also place center seed to guarantee connectivity anchor
  let cx = config.width / 2
  let cy = config.height / 2
  if grid.get(cx, cy) == @types.Wall {
    grid.set(cx, cy, @types.Floor)
    carved += 1
  }
  // DLA: random walkers aggregate when touching existing floor
  let max_attempts = target * config.max_walk_steps
  let mut attempts = 0
  while carved < target && attempts < max_attempts {
    // Spawn particle at random edge of inner area
    let mut px = rng.rand_range(1, config.width - 1)
    let mut py = rng.rand_range(1, config.height - 1)
    for _ in 0..<config.max_walk_steps {
      attempts += 1
      // Check if adjacent to existing floor
      if has_floor_neighbor(grid, px, py) {
        grid.set(px, py, @types.Floor)
        carved += 1
        break
      }
      // Random walk
      let dir = rng.next_int(4)
      match dir {
        0 => if px > 1 { px -= 1 }
        1 => if px < config.width - 2 { px += 1 }
        2 => if py > 1 { py -= 1 }
        _ => if py < config.height - 2 { py += 1 }
      }
    }
  }
  // Ensure connectivity by connecting all floor components
  ensure_connectivity(grid, config, rng)
  // Place Start near center, Goal at farthest floor cell
  grid.set(cx, cy, @types.Start)
  let filled = @types.flood_fill(grid, cx, cy)
  let mut max_dist = 0
  let mut goal : @types.Point = { x: cx, y: cy }
  for p in filled {
    let dist = abs(p.x - cx) + abs(p.y - cy)
    if dist > max_dist {
      max_dist = dist
      goal = p
    }
  }
  grid.set(goal.x, goal.y, @types.Goal)
  { grid, particles: carved }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn has_floor_neighbor(grid : @types.Grid2D, x : Int, y : Int) -> Bool {
  if grid.get(x, y) != @types.Wall {
    return false
  }
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  for d in dirs {
    let nx = x + d.0
    let ny = y + d.1
    if grid.in_bounds(nx, ny) {
      let t = grid.get(nx, ny)
      if t == @types.Floor || t == @types.Start || t == @types.Goal {
        return true
      }
    }
  }
  false
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : DlaConfig,
  rng : @types.RNG
) -> Unit {
  // Find all floor components and connect them
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
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
  // Connect each component to the first (largest)
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
    // Pick random point from each
    let p1 = rng.rand_pick(components[i])
    let p2 = rng.rand_pick(components[largest_idx])
    grid.carve_l_corridor(p1, p2, @types.Floor)
  }
}
