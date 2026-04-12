///|
pub(all) struct DomainWarpConfig {
  width : Int
  height : Int
  scale : Double
  warp_strength : Double
  warp_iterations : Int
  threshold : Double
  octaves : Int
}

///|
pub fn DomainWarpConfig::default() -> DomainWarpConfig {
  {
    width: 50,
    height: 25,
    scale: 0.08,
    warp_strength: 4.0,
    warp_iterations: 2,
    threshold: 0.45,
    octaves: 3,
  }
}

///|
pub(all) struct DomainWarpResult {
  grid : @types.Grid2D
  open_cells : Int
}

///|
pub fn generate(
  config : DomainWarpConfig,
  rng : @types.RNG
) -> DomainWarpResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Generate multiple permutation tables for different noise layers
  let perm1 = generate_perm(rng)
  let perm2 = generate_perm(rng)
  let perm3 = generate_perm(rng)
  let offset_x = rng.next_int(10000).to_double()
  let offset_y = rng.next_int(10000).to_double()
  let mut open_cells = 0
  for y in 0..<config.height {
    for x in 0..<config.width {
      // Border is always wall
      if x == 0 || y == 0 || x == config.width - 1 || y == config.height - 1 {
        continue
      }
      let mut wx = x.to_double() * config.scale + offset_x
      let mut wy = y.to_double() * config.scale + offset_y
      // Apply domain warping iterations
      for _ in 0..<config.warp_iterations {
        let warp_x = fbm(perm1, wx, wy, config.octaves, 0.5) *
          config.warp_strength
        let warp_y = fbm(perm2, wx + 5.2, wy + 1.3, config.octaves, 0.5) *
          config.warp_strength
        wx = wx + warp_x
        wy = wy + warp_y
      }
      // Final noise evaluation
      let value = fbm(perm3, wx, wy, config.octaves, 0.5)
      // Normalize from [-1,1] to [0,1]
      let normalized = (value + 1.0) / 2.0
      if normalized < config.threshold {
        grid.set(x, y, @types.Floor)
        open_cells += 1
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, config.width, config.height)
  // Place Start and Goal
  if open_cells > 0 {
    place_start_goal(grid, config.width, config.height)
  }
  { grid, open_cells }
}

// Value noise implementation

///|
fn generate_perm(rng : @types.RNG) -> Array[Int] {
  let perm = Array::makei(512, fn(i) { i % 256 })
  for i = 255; i > 0; {
    let j = rng.next_int(i + 1)
    perm.swap(i, j)
    continue i - 1
  }
  for i in 0..<256 {
    perm[256 + i] = perm[i]
  }
  perm
}

///|
fn value_noise(perm : Array[Int], x : Double, y : Double) -> Double {
  let xi = x.to_int()
  let yi = y.to_int()
  let xf = x - xi.to_double()
  let yf = y - yi.to_double()
  let u = xf * xf * (3.0 - 2.0 * xf)
  let v = yf * yf * (3.0 - 2.0 * yf)
  let xi_mod = (xi % 256 + 256) % 256
  let yi_mod = (yi % 256 + 256) % 256
  let aa = hash_to_double(perm, xi_mod, yi_mod)
  let ba = hash_to_double(perm, xi_mod + 1, yi_mod)
  let ab = hash_to_double(perm, xi_mod, yi_mod + 1)
  let bb = hash_to_double(perm, xi_mod + 1, yi_mod + 1)
  let x1 = lerp(aa, ba, u)
  let x2 = lerp(ab, bb, u)
  lerp(x1, x2, v)
}

///|
fn hash_to_double(perm : Array[Int], x : Int, y : Int) -> Double {
  let h = perm[perm[x % 256] + y % 256]
  h.to_double() / 255.0 * 2.0 - 1.0
}

///|
fn lerp(a : Double, b : Double, t : Double) -> Double {
  a + (b - a) * t
}

///|
fn fbm(
  perm : Array[Int],
  x : Double,
  y : Double,
  octaves : Int,
  persistence : Double
) -> Double {
  let mut total = 0.0
  let mut amplitude = 1.0
  let mut frequency = 1.0
  let mut max_val = 0.0
  for _ in 0..<octaves {
    total += value_noise(perm, x * frequency, y * frequency) * amplitude
    max_val += amplitude
    amplitude *= persistence
    frequency *= 2.0
  }
  total / max_val
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
  let start = find_nearest_floor(grid, cx, cy, width, height)
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
  cy : Int,
  _width : Int,
  _height : Int
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
