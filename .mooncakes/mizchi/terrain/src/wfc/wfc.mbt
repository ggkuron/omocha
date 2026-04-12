///|
pub(all) struct WfcConfig {
  width : Int
  height : Int
  max_retries : Int
}

///|
pub fn WfcConfig::default() -> WfcConfig {
  { width: 20, height: 12, max_retries: 10 }
}

///|
pub(all) struct WfcResult {
  grid : @types.Grid2D
  success : Bool
  retries : Int
}

// WFC tile types for dungeon generation
// 0=Wall, 1=Floor, 2=Corridor
// Adjacency rules: Floor-Floor OK, Floor-Corridor OK, Corridor-Wall OK
// Wall can be next to anything

///|
priv struct WfcCell {
  possible : Array[Bool] // which tile IDs are possible
  collapsed : Bool
  mut tile_id : Int // -1 if not collapsed
}

///|
fn WfcCell::entropy(self : WfcCell) -> Int {
  let mut count = 0
  for p in self.possible {
    if p {
      count += 1
    }
  }
  count
}

///|
fn make_cell(num_tiles : Int) -> WfcCell {
  { possible: Array::make(num_tiles, true), collapsed: false, tile_id: -1 }
}

// Adjacency rules: can tile_a be adjacent to tile_b?

///|
fn can_be_adjacent(a : Int, b : Int) -> Bool {
  // 0=Wall, 1=Floor, 2=Corridor
  match (a, b) {
    (0, 0) => true // wall-wall
    (0, 1) => true // wall-floor (border)
    (1, 0) => true
    (1, 1) => true // floor-floor
    (1, 2) => true // floor-corridor
    (2, 1) => true
    (2, 2) => true // corridor-corridor
    (0, 2) => true // wall-corridor (border)
    (2, 0) => true
    _ => false
  }
}

///|
fn tile_id_to_tile(id : Int) -> @types.Tile {
  match id {
    0 => @types.Wall
    1 => @types.Floor
    2 => @types.Corridor
    _ => @types.Empty
  }
}

///|
fn find_min_entropy(
  cells : Array[WfcCell],
  width : Int,
  height : Int,
  rng : @types.RNG
) -> (Int, Int)? {
  let mut min_ent = 999
  let candidates : Array[(Int, Int)] = []
  for y in 0..<height {
    for x in 0..<width {
      let cell = cells[y * width + x]
      if cell.collapsed {
        continue
      }
      let e = cell.entropy()
      if e == 0 {
        return None // contradiction
      }
      if e < min_ent {
        min_ent = e
        candidates.clear()
        candidates.push((x, y))
      } else if e == min_ent {
        candidates.push((x, y))
      }
    }
  }
  if candidates.length() == 0 {
    return None // all collapsed
  }
  Some(rng.rand_pick(candidates))
}

///|
fn collapse(cell : WfcCell, rng : @types.RNG) -> Unit {
  let options : Array[Int] = []
  for i, p in cell.possible {
    if p {
      options.push(i)
    }
  }
  if options.length() == 0 {
    return
  }
  let chosen = rng.rand_pick(options)
  for i in 0..<cell.possible.length() {
    cell.possible[i] = i == chosen
  }
  cell.tile_id = chosen
  // Mark as collapsed - need mutable access via array
}

///|
fn propagate(
  cells : Array[WfcCell],
  width : Int,
  height : Int,
  start_x : Int,
  start_y : Int
) -> Bool {
  let stack : Array[(Int, Int)] = [(start_x, start_y)]
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  while stack.length() > 0 {
    let pos = stack.pop().unwrap()
    let cx = pos.0
    let cy = pos.1
    let cell_idx = cy * width + cx
    for d in dirs {
      let nx = cx + d.0
      let ny = cy + d.1
      if nx < 0 || nx >= width || ny < 0 || ny >= height {
        continue
      }
      let neighbor_idx = ny * width + nx
      let neighbor = cells[neighbor_idx]
      if neighbor.collapsed {
        continue
      }
      let mut changed = false
      for tile_b in 0..<neighbor.possible.length() {
        if not(neighbor.possible[tile_b]) {
          continue
        }
        // Check if any tile in current cell can be adjacent to tile_b
        let mut valid = false
        for tile_a in 0..<cells[cell_idx].possible.length() {
          if cells[cell_idx].possible[tile_a] && can_be_adjacent(tile_a, tile_b) {
            valid = true
            break
          }
        }
        if not(valid) {
          neighbor.possible[tile_b] = false
          changed = true
        }
      }
      if changed {
        if neighbor.entropy() == 0 {
          return false // contradiction
        }
        stack.push((nx, ny))
      }
    }
  }
  true
}

///|
fn run_wfc(
  width : Int,
  height : Int,
  rng : @types.RNG
) -> @types.Grid2D? {
  let num_tiles = 3
  let cells : Array[WfcCell] = Array::make(width * height, make_cell(num_tiles))
  // Initialize each cell independently
  for i in 0..<(width * height) {
    cells[i] = make_cell(num_tiles)
  }
  // Force border to be walls
  for x in 0..<width {
    cells[x] = { possible: [true, false, false], collapsed: true, tile_id: 0 }
    cells[(height - 1) * width + x] = {
      possible: [true, false, false],
      collapsed: true,
      tile_id: 0,
    }
  }
  for y in 0..<height {
    cells[y * width] = {
      possible: [true, false, false],
      collapsed: true,
      tile_id: 0,
    }
    cells[y * width + width - 1] = {
      possible: [true, false, false],
      collapsed: true,
      tile_id: 0,
    }
  }
  // Propagate from borders
  for x in 0..<width {
    if not(propagate(cells, width, height, x, 0)) {
      return None
    }
    if not(propagate(cells, width, height, x, height - 1)) {
      return None
    }
  }
  for y in 0..<height {
    if not(propagate(cells, width, height, 0, y)) {
      return None
    }
    if not(propagate(cells, width, height, width - 1, y)) {
      return None
    }
  }
  // Main WFC loop
  let total_inner = (width - 2) * (height - 2)
  for _ in 0..<total_inner {
    let min_pos = find_min_entropy(cells, width, height, rng)
    match min_pos {
      None => break
      Some(pos) => {
        let idx = pos.1 * width + pos.0
        collapse(cells[idx], rng)
        cells[idx] = { ..cells[idx], collapsed: true }
        if not(propagate(cells, width, height, pos.0, pos.1)) {
          return None
        }
      }
    }
  }
  // Build grid
  let grid = @types.Grid2D::make(width, height, @types.Wall)
  for y in 0..<height {
    for x in 0..<width {
      let cell = cells[y * width + x]
      if cell.collapsed && cell.tile_id >= 0 {
        grid.set(x, y, tile_id_to_tile(cell.tile_id))
      } else {
        // Pick first available
        for i, p in cell.possible {
          if p {
            grid.set(x, y, tile_id_to_tile(i))
            break
          }
        }
      }
    }
  }
  Some(grid)
}

///|
pub fn generate(config : WfcConfig, rng : @types.RNG) -> WfcResult {
  for retry in 0..<config.max_retries {
    // Save RNG state by just trying
    let result = run_wfc(config.width, config.height, rng)
    match result {
      Some(grid) => return { grid, success: true, retries: retry }
      None => continue
    }
  }
  // Fallback: return empty grid
  {
    grid: @types.Grid2D::make(config.width, config.height, @types.Wall),
    success: false,
    retries: config.max_retries,
  }
}
