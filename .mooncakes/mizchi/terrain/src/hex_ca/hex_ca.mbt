///|
pub(all) struct HexCaConfig {
  cols : Int
  rows : Int
  initial_fill : Double
  iterations : Int
  birth : Int
  survival : Int
}

///|
pub fn HexCaConfig::default() -> HexCaConfig {
  { cols: 30, rows: 20, initial_fill: 0.45, iterations: 6, birth: 4, survival: 3 }
}

///|
pub(all) struct HexCaResult {
  grid : @types.HexGrid
  open_cells : Int
}

///|
pub fn generate(config : HexCaConfig, rng : @types.RNG) -> HexCaResult {
  let cols = config.cols
  let rows = config.rows
  let total = cols * rows
  // Initialize cells: true = wall, false = floor
  let cells = Array::make(total, false)
  for i in 0..<total {
    cells[i] = rng.rand_bool(config.initial_fill)
  }
  // Border cells are always walls
  for c in 0..<cols {
    cells[c] = true // top row
    cells[(rows - 1) * cols + c] = true // bottom row
  }
  for r in 0..<rows {
    cells[r * cols] = true // left col
    cells[r * cols + (cols - 1)] = true // right col
  }
  // Temporary HexGrid for neighbor queries
  let hex = @types.HexGrid::make(cols, rows, @types.Wall)
  // Iterate cellular automata
  for _iter in 0..<config.iterations {
    let next = Array::make(total, false)
    for r in 0..<rows {
      for c in 0..<cols {
        // Border always wall
        if c == 0 || r == 0 || c == cols - 1 || r == rows - 1 {
          next[r * cols + c] = true
          continue
        }
        // Count wall neighbors using hex adjacency
        let neighbors = hex.neighbors6(c, r)
        let mut wall_count = 0
        for n in neighbors {
          if cells[n.1 * cols + n.0] {
            wall_count += 1
          }
        }
        // Also count out-of-bounds as walls (already handled by in_bounds in neighbors6,
        // but cells near border may have fewer than 6 neighbors)
        let missing = 6 - neighbors.length()
        wall_count += missing
        let is_wall = cells[r * cols + c]
        if is_wall {
          // Survival: wall stays if enough wall neighbors
          next[r * cols + c] = wall_count >= config.survival
        } else {
          // Birth: floor becomes wall if enough wall neighbors
          next[r * cols + c] = wall_count >= config.birth
        }
      }
    }
    // Copy next into cells
    for i in 0..<total {
      cells[i] = next[i]
    }
  }
  // Build HexGrid from cells
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let mut open_count = 0
  for r in 0..<rows {
    for c in 0..<cols {
      if not(cells[r * cols + c]) {
        grid.set(c, r, @types.Floor)
        open_count += 1
      }
    }
  }
  { grid, open_cells: open_count }
}
