///|
pub(all) struct HexGridConfig {
  width : Int
  height : Int
  hex_size : Int
  open_chance : Double
  extra_wall_chance : Double
}

///|
pub fn HexGridConfig::default() -> HexGridConfig {
  { width: 50, height: 25, hex_size: 4, open_chance: 0.5, extra_wall_chance: 0.2 }
}

///|
pub(all) struct HexGridResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  open_cells : Int
}

///|
priv struct HexCell {
  is_open : Bool
}

///|
fn hex_pixel_x(col : Int, row : Int, hex_size : Int) -> Int {
  // Offset coordinates: odd rows shifted right by half a hex
  let base = col * hex_size
  if row % 2 == 1 {
    base + hex_size / 2
  } else {
    base
  }
}

///|
fn hex_pixel_y(row : Int, hex_size : Int) -> Int {
  // Vertical spacing for hex layout (approximate with 3/4 height)
  row * (hex_size * 3 / 4)
}

///|
fn hex_neighbors_offsets(row : Int) -> Array[(Int, Int)] {
  // For offset coordinates (odd-row shifted right):
  if row % 2 == 0 {
    // Even row neighbors
    [(-1, 0), (1, 0), (-1, -1), (0, -1), (-1, 1), (0, 1)]
  } else {
    // Odd row neighbors
    [(-1, 0), (1, 0), (0, -1), (1, -1), (0, 1), (1, 1)]
  }
}

///|
fn carve_hex_cell(
  grid : @types.Grid2D,
  px : Int,
  py : Int,
  hex_size : Int
) -> Unit {
  // Fill a hex-sized rectangle with Floor tiles
  for dy in 0..<hex_size {
    for dx in 0..<hex_size {
      let x = px + dx
      let y = py + dy
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
}

///|
pub fn generate(config : HexGridConfig, rng : @types.RNG) -> HexGridResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let hex_size = config.hex_size
  // Calculate how many hex cells fit in the grid
  let cols = config.width / hex_size
  let rows = config.height / (hex_size * 3 / 4)
  let cols = if cols < 1 { 1 } else { cols }
  let rows = if rows < 1 { 1 } else { rows }
  // Create hex cell grid and randomly mark cells as open or closed
  let cells : Array[Array[HexCell]] = []
  let mut open_count = 0
  for r in 0..<rows {
    let row_cells : Array[HexCell] = []
    for c in 0..<cols {
      let is_open = rng.rand_bool(config.open_chance)
      row_cells.push({ is_open, })
      if is_open {
        open_count += 1
      }
    }
    cells.push(row_cells)
  }
  // Apply extra wall chance: some open cells get turned to walls
  for r in 0..<rows {
    for c in 0..<cols {
      if cells[r][c].is_open && rng.rand_bool(config.extra_wall_chance) {
        cells[r] = cells[r].copy()
        cells[r][c] = { is_open: false }
        open_count -= 1
      }
    }
  }
  // Carve open hex cells into the grid
  for r in 0..<rows {
    for c in 0..<cols {
      if cells[r][c].is_open {
        let px = hex_pixel_x(c, r, hex_size)
        let py = hex_pixel_y(r, hex_size)
        // Ensure the hex cell fits in the grid
        if px + hex_size <= config.width && py + hex_size <= config.height {
          carve_hex_cell(grid, px, py, hex_size)
          let rect : @types.Rect = {
            x: px,
            y: py,
            w: hex_size,
            h: hex_size,
          }
          let room : @types.Room = { rect, id: rooms.length() }
          rooms.push(room)
        }
      }
    }
  }
  // Connect adjacent open hex cells with corridors
  for r in 0..<rows {
    for c in 0..<cols {
      if not(cells[r][c].is_open) {
        continue
      }
      let offsets = hex_neighbors_offsets(r)
      for offset in offsets {
        let nc = c + offset.0
        let nr = r + offset.1
        if nc >= 0 && nc < cols && nr >= 0 && nr < rows {
          if cells[nr][nc].is_open {
            // Only connect in one direction to avoid duplicate corridors
            if nr > r || (nr == r && nc > c) {
              let px1 = hex_pixel_x(c, r, hex_size) + hex_size / 2
              let py1 = hex_pixel_y(r, hex_size) + hex_size / 2
              let px2 = hex_pixel_x(nc, nr, hex_size) + hex_size / 2
              let py2 = hex_pixel_y(nr, hex_size) + hex_size / 2
              let from : @types.Point = { x: px1, y: py1 }
              let to : @types.Point = { x: px2, y: py2 }
              grid.carve_line(from, to, @types.Corridor)
            }
          }
        }
      }
    }
  }
  { grid, rooms, open_cells: open_count }
}
