///|
pub(all) struct DiamondSquareConfig {
  width : Int
  height : Int
  roughness : Double
  threshold : Double
  initial_range : Double
}

///|
pub fn DiamondSquareConfig::default() -> DiamondSquareConfig {
  {
    width: 49,
    height: 25,
    roughness: 0.6,
    threshold: 0.45,
    initial_range: 1.0,
  }
}

///|
pub(all) struct DiamondSquareResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  open_cells : Int
}

///|
pub fn generate(
  config : DiamondSquareConfig,
  rng : @types.RNG
) -> DiamondSquareResult {
  // Find power-of-2 size that covers both dimensions
  let max_dim = config.width.max(config.height)
  let mut size = 2
  while size < max_dim {
    size = size * 2
  }
  size = size + 1 // Diamond-square needs 2^n + 1
  // Height map
  let hmap = Array::make(size * size, 0.0)
  // Seed corners
  hmap[0] = rand_double(rng)
  hmap[size - 1] = rand_double(rng)
  hmap[(size - 1) * size] = rand_double(rng)
  hmap[(size - 1) * size + (size - 1)] = rand_double(rng)
  let mut step = size - 1
  let mut range = config.initial_range
  while step > 1 {
    let half = step / 2
    // Diamond step
    let mut y = 0
    while y < size - 1 {
      let mut x = 0
      while x < size - 1 {
        let tl = hmap[y * size + x]
        let tr = hmap[y * size + (x + step)]
        let bl = hmap[(y + step) * size + x]
        let br = hmap[(y + step) * size + (x + step)]
        let avg = (tl + tr + bl + br) / 4.0
        hmap[(y + half) * size + (x + half)] = avg + rand_offset(rng, range)
        x = x + step
      }
      y = y + step
    }
    // Square step
    y = 0
    while y < size {
      let mut x = if y % step == 0 { half } else { 0 }
      while x < size {
        let mut sum = 0.0
        let mut count = 0
        if y >= half {
          sum = sum + hmap[(y - half) * size + x]
          count = count + 1
        }
        if y + half < size {
          sum = sum + hmap[(y + half) * size + x]
          count = count + 1
        }
        if x >= half {
          sum = sum + hmap[y * size + (x - half)]
          count = count + 1
        }
        if x + half < size {
          sum = sum + hmap[y * size + (x + half)]
          count = count + 1
        }
        if count > 0 {
          hmap[y * size + x] = sum / count.to_double() +
            rand_offset(rng, range)
        }
        x = x + step
      }
      y = y + half
    }
    range = range * config.roughness
    step = half
  }
  // Normalize to 0..1
  let mut min_val = hmap[0]
  let mut max_val = hmap[0]
  for v in hmap {
    if v < min_val { min_val = v }
    if v > max_val { max_val = v }
  }
  let spread = max_val - min_val
  if spread > 0.0001 {
    for i in 0..<hmap.length() {
      hmap[i] = (hmap[i] - min_val) / spread
    }
  }
  // Create grid from heightmap
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let mut open_cells = 0
  for y in 0..<config.height {
    for x in 0..<config.width {
      if x == 0 || y == 0 || x == config.width - 1 || y == config.height - 1 {
        continue
      }
      let h = hmap[y * size + x]
      if h < config.threshold {
        grid.set(x, y, @types.Floor)
        open_cells = open_cells + 1
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, config)
  // Find rooms via flood fill
  let rooms = find_rooms(grid, config)
  // Place Start and Goal
  place_start_goal(grid, config)
  { grid, rooms, open_cells }
}

///|
fn rand_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
fn rand_offset(rng : @types.RNG, range : Double) -> Double {
  (rand_double(rng) * 2.0 - 1.0) * range
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : DiamondSquareConfig
) -> Unit {
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      let t = grid.get(x, y)
      if not(visited[idx]) && (t == @types.Floor || t == @types.Door) {
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

///|
fn find_rooms(
  grid : @types.Grid2D,
  config : DiamondSquareConfig
) -> Array[@types.Room] {
  let visited = Array::make(config.width * config.height, false)
  let rooms : Array[@types.Room] = []
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
        if comp.length() > 4 {
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
  }
  rooms
}

///|
fn place_start_goal(
  grid : @types.Grid2D,
  config : DiamondSquareConfig
) -> Unit {
  let mut start_set = false
  let mut goal_set = false
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if not(start_set) && grid.get(x, y) == @types.Floor {
        grid.set(x, y, @types.Start)
        start_set = true
      }
    }
  }
  for y = config.height - 2; y > 0; y = y - 1 {
    for x = config.width - 2; x > 0; x = x - 1 {
      if not(goal_set) && grid.get(x, y) == @types.Floor {
        grid.set(x, y, @types.Goal)
        goal_set = true
      }
    }
  }
}
