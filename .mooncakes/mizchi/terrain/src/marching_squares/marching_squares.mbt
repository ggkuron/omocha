///|
pub(all) struct MarchingSquaresConfig {
  width : Int
  height : Int
  initial_fill : Double
  ca_iterations : Int
  birth_threshold : Int
  survival_threshold : Int
  smooth : Bool
}

///|
pub fn MarchingSquaresConfig::default() -> MarchingSquaresConfig {
  {
    width: 50,
    height: 25,
    initial_fill: 0.45,
    ca_iterations: 5,
    birth_threshold: 5,
    survival_threshold: 4,
    smooth: true,
  }
}

///|
pub(all) struct MarchingSquaresResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  open_cells : Int
}

// Run cellular automata (same algorithm as cellular.mbt)
///|
fn run_cellular_automata(
  grid : @types.Grid2D,
  config : MarchingSquaresConfig,
  rng : @types.RNG
) -> Unit {
  // Random initial fill
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if rng.rand_bool(config.initial_fill) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Cellular automata iterations
  for _ in 0..<config.ca_iterations {
    let old = grid.clone()
    for y in 1..<(config.height - 1) {
      for x in 1..<(config.width - 1) {
        let walls = old.count_neighbors(x, y, @types.Wall)
        if old.get(x, y) == @types.Wall {
          // Birth rule: wall becomes floor if fewer walls around
          if walls < config.birth_threshold {
            grid.set(x, y, @types.Floor)
          }
        } else {
          // Death rule: floor becomes wall if too many walls
          if walls >= config.survival_threshold {
            grid.set(x, y, @types.Wall)
          }
        }
      }
    }
  }
}

// Compute marching squares index for the 2x2 block starting at (x, y)
// Bit layout:
//   bit0 = top-left (x, y)
//   bit1 = top-right (x+1, y)
//   bit2 = bottom-left (x, y+1)
//   bit3 = bottom-right (x+1, y+1)
// A bit is 1 if the cell is Floor (or Corridor), 0 if Wall
///|
fn is_open(grid : @types.Grid2D, x : Int, y : Int) -> Bool {
  if not(grid.in_bounds(x, y)) {
    return false
  }
  let tile = grid.get(x, y)
  tile == @types.Floor || tile == @types.Corridor
}

///|
fn marching_index(grid : @types.Grid2D, x : Int, y : Int) -> Int {
  let mut idx = 0
  if is_open(grid, x, y) {
    idx = idx + 1 // bit 0
  }
  if is_open(grid, x + 1, y) {
    idx = idx + 2 // bit 1
  }
  if is_open(grid, x, y + 1) {
    idx = idx + 4 // bit 2
  }
  if is_open(grid, x + 1, y + 1) {
    idx = idx + 8 // bit 3
  }
  idx
}

// Apply marching squares smoothing
// For each 2x2 block, determine if transition cells should be
// smoothed into corridors
///|
fn apply_marching_squares(grid : @types.Grid2D) -> Unit {
  // We need a copy to read from while writing
  let source = grid.clone()
  // Process each 2x2 block
  for y in 0..<(grid.height - 1) {
    for x in 0..<(grid.width - 1) {
      let idx = marching_index(source, x, y)
      match idx {
        0 => {
          // 0000: all wall -> keep as wall
          ()
        }
        15 => {
          // 1111: all floor -> keep as floor
          ()
        }
        1 => {
          // 0001: only top-left is floor
          // Smooth: make it a corridor transition
          if grid.in_bounds(x, y) {
            grid.set(x, y, @types.Corridor)
          }
        }
        2 => {
          // 0010: only top-right is floor
          if grid.in_bounds(x + 1, y) {
            grid.set(x + 1, y, @types.Corridor)
          }
        }
        4 => {
          // 0100: only bottom-left is floor
          if grid.in_bounds(x, y + 1) {
            grid.set(x, y + 1, @types.Corridor)
          }
        }
        8 => {
          // 1000: only bottom-right is floor
          if grid.in_bounds(x + 1, y + 1) {
            grid.set(x + 1, y + 1, @types.Corridor)
          }
        }
        3 => {
          // 0011: top row is floor -> corridor for transition
          if grid.in_bounds(x, y) {
            grid.set(x, y, @types.Corridor)
          }
          if grid.in_bounds(x + 1, y) {
            grid.set(x + 1, y, @types.Corridor)
          }
        }
        5 => {
          // 0101: left column is floor -> corridor
          if grid.in_bounds(x, y) {
            grid.set(x, y, @types.Corridor)
          }
          if grid.in_bounds(x, y + 1) {
            grid.set(x, y + 1, @types.Corridor)
          }
        }
        10 => {
          // 1010: right column is floor -> corridor
          if grid.in_bounds(x + 1, y) {
            grid.set(x + 1, y, @types.Corridor)
          }
          if grid.in_bounds(x + 1, y + 1) {
            grid.set(x + 1, y + 1, @types.Corridor)
          }
        }
        12 => {
          // 1100: bottom row is floor -> corridor
          if grid.in_bounds(x, y + 1) {
            grid.set(x, y + 1, @types.Corridor)
          }
          if grid.in_bounds(x + 1, y + 1) {
            grid.set(x + 1, y + 1, @types.Corridor)
          }
        }
        6 => {
          // 0110: diagonal (top-right, bottom-left) -> corridor both
          if grid.in_bounds(x + 1, y) {
            grid.set(x + 1, y, @types.Corridor)
          }
          if grid.in_bounds(x, y + 1) {
            grid.set(x, y + 1, @types.Corridor)
          }
        }
        9 => {
          // 1001: diagonal (top-left, bottom-right) -> corridor both
          if grid.in_bounds(x, y) {
            grid.set(x, y, @types.Corridor)
          }
          if grid.in_bounds(x + 1, y + 1) {
            grid.set(x + 1, y + 1, @types.Corridor)
          }
        }
        7 => {
          // 0111: all but bottom-right -> make bottom-right corridor
          if grid.in_bounds(x + 1, y + 1) && grid.get(x + 1, y + 1) == @types.Wall {
            grid.set(x + 1, y + 1, @types.Corridor)
          }
        }
        11 => {
          // 1011: all but bottom-left -> make bottom-left corridor
          if grid.in_bounds(x, y + 1) && grid.get(x, y + 1) == @types.Wall {
            grid.set(x, y + 1, @types.Corridor)
          }
        }
        13 => {
          // 1101: all but top-right -> make top-right corridor
          if grid.in_bounds(x + 1, y) && grid.get(x + 1, y) == @types.Wall {
            grid.set(x + 1, y, @types.Corridor)
          }
        }
        14 => {
          // 1110: all but top-left -> make top-left corridor
          if grid.in_bounds(x, y) && grid.get(x, y) == @types.Wall {
            grid.set(x, y, @types.Corridor)
          }
        }
        _ => ()
      }
    }
  }
}

// Smooth pass: any wall surrounded by 6+ floor/corridor neighbors becomes floor
///|
fn smooth_pass(grid : @types.Grid2D) -> Unit {
  let old = grid.clone()
  for y in 1..<(grid.height - 1) {
    for x in 1..<(grid.width - 1) {
      if old.get(x, y) != @types.Wall {
        continue
      }
      let mut open_count = 0
      for dy in -1..=1 {
        for dx in -1..=1 {
          if dx == 0 && dy == 0 {
            continue
          }
          let nx = x + dx
          let ny = y + dy
          if grid.in_bounds(nx, ny) {
            let t = old.get(nx, ny)
            if t == @types.Floor || t == @types.Corridor {
              open_count += 1
            }
          }
        }
      }
      if open_count >= 6 {
        grid.set(x, y, @types.Floor)
      }
    }
  }
}

// Flood fill to find connected regions (for room detection)
///|
fn flood_fill_region(
  grid : @types.Grid2D,
  visited : Array[Bool],
  start_x : Int,
  start_y : Int
) -> @types.Rect {
  let w = grid.width
  let stack : Array[(Int, Int)] = [(start_x, start_y)]
  visited[start_y * w + start_x] = true
  let mut min_x = start_x
  let mut max_x = start_x
  let mut min_y = start_y
  let mut max_y = start_y
  while stack.length() > 0 {
    let pos = stack.pop().unwrap()
    let cx = pos.0
    let cy = pos.1
    if cx < min_x {
      min_x = cx
    }
    if cx > max_x {
      max_x = cx
    }
    if cy < min_y {
      min_y = cy
    }
    if cy > max_y {
      max_y = cy
    }
    // Check 4 neighbors
    let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for d in dirs {
      let nx = cx + d.0
      let ny = cy + d.1
      if not(grid.in_bounds(nx, ny)) {
        continue
      }
      let idx = ny * w + nx
      if visited[idx] {
        continue
      }
      let tile = grid.get(nx, ny)
      if tile == @types.Floor || tile == @types.Corridor {
        visited[idx] = true
        stack.push((nx, ny))
      }
    }
  }
  { x: min_x, y: min_y, w: max_x - min_x + 1, h: max_y - min_y + 1 }
}

// Find rooms as connected regions of open cells
///|
fn find_rooms(grid : @types.Grid2D) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let visited = Array::make(grid.width * grid.height, false)
  let mut room_id = 0
  for y in 0..<grid.height {
    for x in 0..<grid.width {
      let idx = y * grid.width + x
      if visited[idx] {
        continue
      }
      let tile = grid.get(x, y)
      if tile != @types.Floor && tile != @types.Corridor {
        continue
      }
      let rect = flood_fill_region(grid, visited, x, y)
      // Only count regions with area >= 4 as rooms
      if rect.w * rect.h >= 4 {
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  rooms
}

///|
fn count_open_cells(grid : @types.Grid2D) -> Int {
  let mut count = 0
  for y in 0..<grid.height {
    for x in 0..<grid.width {
      let tile = grid.get(x, y)
      if tile == @types.Floor || tile == @types.Corridor {
        count += 1
      }
    }
  }
  count
}

///|
pub fn generate(
  config : MarchingSquaresConfig,
  rng : @types.RNG
) -> MarchingSquaresResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Step 1: Run standard cellular automata
  run_cellular_automata(grid, config, rng)
  // Step 2: Apply marching squares smoothing
  apply_marching_squares(grid)
  // Step 3: Optional smooth pass
  if config.smooth {
    smooth_pass(grid)
  }
  // Find rooms and count open cells
  let rooms = find_rooms(grid)
  let open_cells = count_open_cells(grid)
  { grid, rooms, open_cells }
}
