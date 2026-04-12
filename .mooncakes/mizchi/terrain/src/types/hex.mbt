///| HexGrid — odd-r offset座標の六角グリッド
pub(all) struct HexGrid {
  cols : Int
  rows : Int
  cells : Array[Tile]
  orientation : Int // 0=pointy-top, 1=flat-top
}

///|
pub fn HexGrid::make(
  cols : Int,
  rows : Int,
  fill : Tile,
  orientation~ : Int = 0
) -> HexGrid {
  { cols, rows, cells: Array::make(cols * rows, fill), orientation }
}

///|
pub fn HexGrid::in_bounds(self : HexGrid, col : Int, row : Int) -> Bool {
  col >= 0 && col < self.cols && row >= 0 && row < self.rows
}

///|
pub fn HexGrid::get(self : HexGrid, col : Int, row : Int) -> Tile {
  self.cells[row * self.cols + col]
}

///|
pub fn HexGrid::set(
  self : HexGrid,
  col : Int,
  row : Int,
  tile : Tile
) -> Unit {
  self.cells[row * self.cols + col] = tile
}

///| odd-r offset座標での6方向隣接セル
pub fn HexGrid::neighbors6(
  self : HexGrid,
  col : Int,
  row : Int
) -> Array[(Int, Int)] {
  let result : Array[(Int, Int)] = []
  let parity = row % 2
  // odd-r offset: even rows and odd rows have different neighbor offsets
  let dirs : Array[Array[(Int, Int)]] = [
    // even row
    [(-1, -1), (0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1)],
    // odd row
    [(0, -1), (1, -1), (-1, 0), (1, 0), (0, 1), (1, 1)],
  ]
  for d in dirs[parity] {
    let nc = col + d.0
    let nr = row + d.1
    if self.in_bounds(nc, nr) {
      result.push((nc, nr))
    }
  }
  result
}

///|
pub fn HexGrid::to_ascii(self : HexGrid) -> String {
  let buf = StringBuilder::new()
  for row in 0..<self.rows {
    if row % 2 == 1 {
      buf.write_char(' ')
    }
    for col in 0..<self.cols {
      buf.write_char(self.get(col, row).to_char())
      if col < self.cols - 1 {
        buf.write_char(' ')
      }
    }
    if row < self.rows - 1 {
      buf.write_char('\n')
    }
  }
  buf.to_string()
}

///| HexGridをGrid2Dに変換（各hexセルをhex_size四方のタイルとして展開）
pub fn HexGrid::to_grid2d(self : HexGrid, hex_size : Int) -> Grid2D {
  let w = self.cols * hex_size + hex_size / 2
  let h = self.rows * hex_size
  let grid = Grid2D::make(w, h, Wall)
  for row in 0..<self.rows {
    for col in 0..<self.cols {
      let tile = self.get(col, row)
      if tile != Wall {
        let ox = col * hex_size + (if row % 2 == 1 { hex_size / 2 } else { 0 })
        let oy = row * hex_size
        for dy in 0..<hex_size {
          for dx in 0..<hex_size {
            let gx = ox + dx
            let gy = oy + dy
            if grid.in_bounds(gx, gy) {
              grid.set(gx, gy, tile)
            }
          }
        }
      }
    }
  }
  grid
}
