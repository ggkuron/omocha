///|
pub(all) struct MidpointDisplacementConfig {
  width : Int
  height : Int
  roughness : Double
  num_layers : Int
  threshold : Double
}

///|
pub fn MidpointDisplacementConfig::default() -> MidpointDisplacementConfig {
  { width: 50, height: 25, roughness: 0.5, num_layers: 3, threshold: 0.5 }
}

///|
pub(all) struct MidpointDisplacementResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  open_cells : Int
}

///|
pub fn generate(
  config : MidpointDisplacementConfig,
  rng : @types.RNG
) -> MidpointDisplacementResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Generate ceiling and floor lines from layered midpoint displacement
  let ceiling = Array::make(w, 0)
  let floor_line = Array::make(w, h - 1)
  // Generate layers and combine them
  for layer in 0..<config.num_layers {
    let line = midpoint_displacement_1d(w, config.roughness, rng)
    // Each layer modulates the ceiling or floor
    if layer % 2 == 0 {
      // Modulate ceiling: push it down
      for x in 0..<w {
        let offset = (line[x] * (h.to_double() * 0.3)).to_int()
        let new_ceil = ceiling[x] + offset.max(1)
        ceiling[x] = new_ceil.min(h - 2)
      }
    } else {
      // Modulate floor: push it up
      for x in 0..<w {
        let offset = (line[x] * (h.to_double() * 0.3)).to_int()
        let new_floor = floor_line[x] - offset.max(1)
        floor_line[x] = new_floor.max(1)
      }
    }
  }
  // Ensure ceiling is always above floor with minimum gap
  let min_gap = 3
  for x in 0..<w {
    if ceiling[x] < 1 {
      ceiling[x] = 1
    }
    if floor_line[x] > h - 2 {
      floor_line[x] = h - 2
    }
    if floor_line[x] - ceiling[x] < min_gap {
      let mid = (ceiling[x] + floor_line[x]) / 2
      ceiling[x] = (mid - min_gap / 2).max(1)
      floor_line[x] = (mid + min_gap / 2).min(h - 2)
    }
  }
  // Fill grid: cells between ceiling and floor are Floor
  let mut open_cells = 0
  for x in 0..<w {
    for y in 0..<h {
      // Border walls
      if x == 0 || x == w - 1 || y == 0 || y == h - 1 {
        continue
      }
      if y > ceiling[x] && y < floor_line[x] {
        grid.set(x, y, @types.Floor)
        open_cells += 1
      }
    }
  }
  // Apply smoothing pass: remove isolated walls inside the cave
  let old = grid.clone()
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if old.get(x, y) == @types.Wall {
        let floor_neighbors = 8 - old.count_neighbors(x, y, @types.Wall)
        if floor_neighbors >= 6 {
          grid.set(x, y, @types.Floor)
          open_cells += 1
        }
      }
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, w, h)
  // Find rooms via flood fill
  let rooms = find_rooms(grid, w, h)
  // Place Start and Goal
  place_start_goal(grid, w, h)
  { grid, rooms, open_cells }
}

///|
fn midpoint_displacement_1d(
  length : Int,
  roughness : Double,
  rng : @types.RNG
) -> Array[Double] {
  // Find power of 2 that covers the length
  let mut size = 2
  while size < length {
    size = size * 2
  }
  let points = Array::make(size + 1, 0.0)
  // Set endpoints
  points[0] = rand_double(rng)
  points[size] = rand_double(rng)
  let mut step = size
  let mut displacement = 1.0
  while step > 1 {
    let half = step / 2
    // Subdivide: set midpoint between each pair
    let mut i = half
    while i < size {
      let left = points[i - half]
      let right = points[i + half]
      points[i] = (left + right) / 2.0 + rand_offset(rng, displacement)
      i += step
    }
    displacement *= roughness
    step = half
  }
  // Normalize to 0..1
  let mut min_val = points[0]
  let mut max_val = points[0]
  for v in points {
    if v < min_val {
      min_val = v
    }
    if v > max_val {
      max_val = v
    }
  }
  let spread = max_val - min_val
  if spread > 0.0001 {
    for i in 0..<points.length() {
      points[i] = (points[i] - min_val) / spread
    }
  }
  // Trim to requested length
  let result = Array::make(length, 0.0)
  for i in 0..<length {
    result[i] = points[i]
  }
  result
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
fn ensure_connectivity(grid : @types.Grid2D, w : Int, h : Int) -> Unit {
  let visited = Array::make(w * h, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let idx = y * w + x
      let t = grid.get(x, y)
      if not(visited[idx]) && (t == @types.Floor || t == @types.Corridor) {
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
    let a = components[i][0]
    let b = components[largest_idx][0]
    grid.carve_l_corridor(a, b, @types.Corridor)
  }
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
        let comp = @types.flood_fill(grid, x, y)
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
