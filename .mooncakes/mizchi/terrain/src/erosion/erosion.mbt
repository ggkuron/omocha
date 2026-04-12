///|
pub(all) struct ErosionConfig {
  width : Int
  height : Int
  num_drops : Int
  erosion_strength : Double
  deposition_rate : Double
  threshold : Double
}

///|
pub fn ErosionConfig::default() -> ErosionConfig {
  {
    width: 50,
    height: 25,
    num_drops: 500,
    erosion_strength: 0.1,
    deposition_rate: 0.05,
    threshold: 0.45,
  }
}

///|
pub(all) struct ErosionResult {
  grid : @types.Grid2D
  rivers : Int
}

///|
pub fn generate(config : ErosionConfig, rng : @types.RNG) -> ErosionResult {
  let w = config.width
  let h = config.height
  // Generate initial heightmap using value noise
  let hmap = generate_heightmap(w, h, rng)
  // Track how many drops pass through each cell
  let flow_count = Array::make(w * h, 0)
  // Simulate water drops
  for _ in 0..<config.num_drops {
    simulate_drop(hmap, flow_count, w, h, config.erosion_strength, config.deposition_rate, rng)
  }
  // Normalize heightmap to 0..1
  normalize(hmap)
  // Find threshold for flow to become water
  // Use top 10% of flow cells as rivers
  let mut max_flow = 0
  for i in 0..<flow_count.length() {
    if flow_count[i] > max_flow {
      max_flow = flow_count[i]
    }
  }
  let flow_threshold = (max_flow.to_double() * 0.3).to_int().max(2)
  // Build grid
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let mut rivers = 0
  for y in 0..<h {
    for x in 0..<w {
      // Border is always wall
      if x == 0 || y == 0 || x == w - 1 || y == h - 1 {
        continue
      }
      let idx = y * w + x
      let height = hmap[idx]
      if flow_count[idx] >= flow_threshold {
        grid.set(x, y, @types.Water)
        rivers += 1
      } else if height < config.threshold {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, w, h)
  // Place Start and Goal
  place_start_goal(grid, w, h)
  { grid, rivers }
}

///|
fn generate_heightmap(w : Int, h : Int, rng : @types.RNG) -> Array[Double] {
  let hmap = Array::make(w * h, 0.0)
  // Generate permutation table for value noise
  let perm = generate_perm(rng)
  let offset_x = rng.next_int(10000).to_double()
  let offset_y = rng.next_int(10000).to_double()
  let scale = 0.08
  for y in 0..<h {
    for x in 0..<w {
      let nx = x.to_double() * scale + offset_x
      let ny = y.to_double() * scale + offset_y
      let value = fbm(perm, nx, ny, 4, 0.5)
      hmap[y * w + x] = (value + 1.0) / 2.0
    }
  }
  hmap
}

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
fn normalize(hmap : Array[Double]) -> Unit {
  let mut min_val = hmap[0]
  let mut max_val = hmap[0]
  for v in hmap {
    if v < min_val {
      min_val = v
    }
    if v > max_val {
      max_val = v
    }
  }
  let spread = max_val - min_val
  if spread > 0.0001 {
    for i in 0..<hmap.length() {
      hmap[i] = (hmap[i] - min_val) / spread
    }
  }
}

///|
fn simulate_drop(
  hmap : Array[Double],
  flow_count : Array[Int],
  w : Int,
  h : Int,
  erosion_strength : Double,
  deposition_rate : Double,
  rng : @types.RNG
) -> Unit {
  let mut x = rng.rand_range(1, w - 1)
  let mut y = rng.rand_range(1, h - 1)
  let mut sediment = 0.0
  let mut prev_height = hmap[y * w + x]
  let max_steps = w + h
  for _ in 0..<max_steps {
    let idx = y * w + x
    flow_count[idx] += 1
    let current_height = hmap[idx]
    // Find steepest descent among 4 neighbors
    let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    let mut best_dx = 0
    let mut best_dy = 0
    let mut best_h = current_height
    for d in dirs {
      let nx = x + d.0
      let ny = y + d.1
      if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 {
        let nh = hmap[ny * w + nx]
        if nh < best_h {
          best_h = nh
          best_dx = d.0
          best_dy = d.1
        }
      }
    }
    // If no downhill neighbor, deposit sediment and stop
    if best_dx == 0 && best_dy == 0 {
      hmap[idx] = hmap[idx] + sediment * deposition_rate
      break
    }
    // Compute height difference
    let height_diff = current_height - best_h
    // Erode current cell
    let erode_amount = height_diff * erosion_strength
    hmap[idx] = hmap[idx] - erode_amount
    sediment += erode_amount
    // If moving uphill relative to previous step, deposit some sediment
    if current_height > prev_height {
      let deposit = sediment * deposition_rate
      hmap[idx] = hmap[idx] + deposit
      sediment -= deposit
    }
    prev_height = current_height
    // Move to best neighbor
    x += best_dx
    y += best_dy
    // Boundary check
    if x <= 0 || x >= w - 1 || y <= 0 || y >= h - 1 {
      break
    }
  }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn ensure_connectivity(grid : @types.Grid2D, w : Int, h : Int) -> Unit {
  let visited = Array::make(w * h, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let idx = y * w + x
      let t = grid.get(x, y)
      if not(visited[idx]) &&
        (t == @types.Floor || t == @types.Water || t == @types.Corridor) {
        let comp = @types.flood_fill(grid, x, y)
        for p in comp {
          visited[p.y * w + p.x] = true
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
    // Find closest pair between component i and largest
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
        let dist = abs(a.x - b.x) + abs(a.y - b.y)
        if dist < best_dist {
          best_dist = dist
          best_a = a
          best_b = b
        }
        jj += step_l
      }
      ii += step_i
    }
    grid.carve_l_corridor(best_a, best_b, @types.Corridor)
  }
}

///|
fn place_start_goal(grid : @types.Grid2D, w : Int, h : Int) -> Unit {
  let mut start_set = false
  let mut goal_set = false
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if not(start_set) && grid.get(x, y) == @types.Floor {
        grid.set(x, y, @types.Start)
        start_set = true
      }
    }
  }
  for y = h - 2; y > 0; y = y - 1 {
    for x = w - 2; x > 0; x = x - 1 {
      if not(goal_set) && grid.get(x, y) == @types.Floor {
        grid.set(x, y, @types.Goal)
        goal_set = true
      }
    }
  }
}
