///|
pub(all) struct HexSpiralConfig {
  cols : Int
  rows : Int
  spacing : Int
  room_chance : Double
}

///|
pub fn HexSpiralConfig::default() -> HexSpiralConfig {
  { cols: 30, rows: 25, spacing: 2, room_chance: 0.2 }
}

///|
pub(all) struct HexSpiralResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : HexSpiralConfig, rng : @types.RNG) -> HexSpiralResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  let center_col = cols / 2
  let center_row = rows / 2
  // Start from center
  grid.set(center_col, center_row, @types.Start)
  // Spiral outward using hex directions
  // In odd-r hex grid, we cycle through 6 directions
  // Direction order for spiral: E, SE, SW, W, NW, NE (clockwise)
  let mut cur_col = center_col
  let mut cur_row = center_row
  let mut ring = 1
  let max_ring = (if cols > rows { cols } else { rows }) / 2
  let mut step_count = 0
  let spacing = if config.spacing < 1 { 1 } else { config.spacing }
  while ring <= max_ring {
    // Move to start of ring: go one step in direction 4 (NW for even, adjusted for odd)
    let start = move_hex(cur_col, cur_row, 4)
    cur_col = start.0
    cur_row = start.1
    // Traverse ring: 6 sides, each side has `ring` steps
    for side in 0..<6 {
      for _step in 0..<ring {
        if grid.in_bounds(cur_col, cur_row) {
          step_count += 1
          if step_count % spacing == 0 {
            grid.set(cur_col, cur_row, @types.Floor)
            // Connect to previous cell via corridor
            let prev = move_hex(cur_col, cur_row, (side + 3) % 6)
            if grid.in_bounds(prev.0, prev.1) &&
              grid.get(prev.0, prev.1) == @types.Wall {
              grid.set(prev.0, prev.1, @types.Corridor)
            }
            // Room chance
            if rng.rand_bool(config.room_chance) {
              create_room(grid, cur_col, cur_row, rooms, rng)
            }
          }
        }
        let next = move_hex(cur_col, cur_row, side)
        cur_col = next.0
        cur_row = next.1
      }
    }
    ring += 1
  }
  // Place goal at last valid spiral cell
  let mut goal_placed = false
  for r = rows - 1; r >= 0; {
    for c = cols - 1; c >= 0; {
      if grid.in_bounds(c, r) && grid.get(c, r) == @types.Floor {
        grid.set(c, r, @types.Goal)
        goal_placed = true
        break
      }
      continue c - 1
    }
    if goal_placed {
      break
    }
    continue r - 1
  }
  { grid, rooms }
}

///|
fn move_hex(col : Int, row : Int, direction : Int) -> (Int, Int) {
  let parity = row & 1
  // Directions: 0=E, 1=SE, 2=SW, 3=W, 4=NW, 5=NE
  let even_dirs : Array[(Int, Int)] = [
    (1, 0), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1),
  ]
  let odd_dirs : Array[(Int, Int)] = [
    (1, 0), (1, 1), (0, 1), (-1, 0), (0, -1), (1, -1),
  ]
  let dirs = if parity == 0 { even_dirs } else { odd_dirs }
  let d = dirs[direction % 6]
  (col + d.0, row + d.1)
}

///|
fn create_room(
  grid : @types.HexGrid,
  col : Int,
  row : Int,
  rooms : Array[@types.Room],
  rng : @types.RNG
) -> Unit {
  // Create a small room by opening this cell and some neighbors
  grid.set(col, row, @types.Floor)
  let neighbors = grid.neighbors6(col, row)
  let count = rng.rand_range(2, neighbors.length() + 1)
  let mut opened = 0
  for n in neighbors {
    if opened >= count {
      break
    }
    if grid.in_bounds(n.0, n.1) && grid.get(n.0, n.1) == @types.Wall {
      grid.set(n.0, n.1, @types.Floor)
      opened += 1
    }
  }
  let rect : @types.Rect = { x: col, y: row, w: 1, h: 1 }
  rooms.push({ rect, id: rooms.length() })
}
