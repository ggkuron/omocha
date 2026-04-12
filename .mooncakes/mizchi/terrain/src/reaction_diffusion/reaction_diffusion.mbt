///|
pub(all) struct ReactionDiffusionConfig {
  width : Int
  height : Int
  iterations : Int
  du : Double
  dv : Double
  feed : Double
  kill : Double
  threshold : Double
  dt : Double
}

///|
pub fn ReactionDiffusionConfig::default() -> ReactionDiffusionConfig {
  {
    width: 50,
    height: 25,
    iterations: 3000,
    du: 1.0,
    dv: 0.5,
    feed: 0.055,
    kill: 0.062,
    threshold: 0.2,
    dt: 1.0,
  }
}

///|
pub(all) struct ReactionDiffusionResult {
  grid : @types.Grid2D
  open_cells : Int
}

///|
pub fn generate(
  config : ReactionDiffusionConfig,
  rng : @types.RNG
) -> ReactionDiffusionResult {
  let w = config.width
  let h = config.height
  let size = w * h
  // Initialize U and V grids
  let u = Array::make(size, 1.0)
  let v = Array::make(size, 0.0)
  // Seed random spots with V
  let num_seeds = 5 + rng.next_int(6)
  for _ in 0..<num_seeds {
    let cx = rng.rand_range(3, w - 3)
    let cy = rng.rand_range(3, h - 3)
    // Place a small square of V
    for dy in -2..=2 {
      for dx in -2..=2 {
        let nx = cx + dx
        let ny = cy + dy
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
          let idx = ny * w + nx
          v[idx] = 1.0
          u[idx] = 0.5
        }
      }
    }
  }
  // Temporary buffers
  let new_u = Array::make(size, 0.0)
  let new_v = Array::make(size, 0.0)
  // Run simulation
  for _ in 0..<config.iterations {
    for y in 0..<h {
      for x in 0..<w {
        let idx = y * w + x
        let u_val = u[idx]
        let v_val = v[idx]
        // Compute Laplacian using 5-point stencil
        let lap_u = laplacian(u, x, y, w, h)
        let lap_v = laplacian(v, x, y, w, h)
        let uvv = u_val * v_val * v_val
        new_u[idx] = u_val +
          (config.du * lap_u - uvv + config.feed * (1.0 - u_val)) * config.dt
        new_v[idx] = v_val +
          (config.dv * lap_v + uvv - (config.kill + config.feed) * v_val) *
            config.dt
        // Clamp values
        if new_u[idx] < 0.0 {
          new_u[idx] = 0.0
        }
        if new_u[idx] > 1.0 {
          new_u[idx] = 1.0
        }
        if new_v[idx] < 0.0 {
          new_v[idx] = 0.0
        }
        if new_v[idx] > 1.0 {
          new_v[idx] = 1.0
        }
      }
    }
    // Swap buffers
    for i in 0..<size {
      u[i] = new_u[i]
      v[i] = new_v[i]
    }
  }
  // Convert V values to tiles
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let mut open_cells = 0
  for y in 0..<h {
    for x in 0..<w {
      // Border is always wall
      if x == 0 || y == 0 || x == w - 1 || y == h - 1 {
        continue
      }
      let idx = y * w + x
      if v[idx] > config.threshold {
        grid.set(x, y, @types.Floor)
        open_cells += 1
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, w, h)
  // Place Start and Goal
  if open_cells > 0 {
    place_start_goal(grid, w, h)
  }
  { grid, open_cells }
}

///|
fn laplacian(
  field : Array[Double],
  x : Int,
  y : Int,
  w : Int,
  h : Int
) -> Double {
  let idx = y * w + x
  let center = field[idx]
  // Use wrapped boundary (toroidal)
  let left = field[y * w + ((x - 1 + w) % w)]
  let right = field[y * w + ((x + 1) % w)]
  let up = field[((y - 1 + h) % h) * w + x]
  let down = field[((y + 1) % h) * w + x]
  left + right + up + down - 4.0 * center
}

///|
fn abs_int(x : Int) -> Int {
  if x < 0 {
    -x
  } else {
    x
  }
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  width : Int,
  height : Int
) -> Unit {
  let visited = Array::make(width * height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(height - 1) {
    for x in 1..<(width - 1) {
      let idx = y * width + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        let comp = @types.flood_fill(grid, x, y)
        for p in comp {
          visited[p.y * width + p.x] = true
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
    let mut best_dist = 999999
    let mut best_a : @types.Point = { x: 0, y: 0 }
    let mut best_b : @types.Point = { x: 0, y: 0 }
    let step_i = (components[i].length() / 20).max(1)
    let step_l = (components[largest_idx].length() / 20).max(1)
    let mut ii = 0
    while ii < components[i].length() {
      let a = components[i][ii]
      let mut jj = 0
      while jj < components[largest_idx].length() {
        let b = components[largest_idx][jj]
        let dist = abs_int(a.x - b.x) + abs_int(a.y - b.y)
        if dist < best_dist {
          best_dist = dist
          best_a = a
          best_b = b
        }
        jj += step_l
      }
      ii += step_i
    }
    grid.carve_l_corridor(best_a, best_b, @types.Floor)
  }
}

///|
fn place_start_goal(grid : @types.Grid2D, width : Int, height : Int) -> Unit {
  let cx = width / 2
  let cy = height / 2
  let start = find_nearest_floor(grid, cx, cy)
  grid.set(start.x, start.y, @types.Start)
  let filled = @types.flood_fill(grid, start.x, start.y)
  let mut max_dist = 0
  let mut goal = start
  for p in filled {
    let dist = abs_int(p.x - start.x) + abs_int(p.y - start.y)
    if dist > max_dist {
      max_dist = dist
      goal = p
    }
  }
  grid.set(goal.x, goal.y, @types.Goal)
}

///|
fn find_nearest_floor(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        if abs_int(dx) + abs_int(dy) != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && grid.get(x, y) == @types.Floor {
          return { x, y }
        }
      }
    }
  }
  { x: cx, y: cy }
}
