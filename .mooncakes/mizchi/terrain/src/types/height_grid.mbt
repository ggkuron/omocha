///| HeightGrid — 高さ付き2Dグリッド（クォータービュー用）
pub(all) struct HeightGrid {
  width : Int
  height : Int
  cells : Array[Tile]
  heights : Array[Int]
  max_height : Int
}

///|
pub fn HeightGrid::make(
  width : Int,
  height : Int,
  fill : Tile,
  max_height : Int
) -> HeightGrid {
  let size = width * height
  {
    width,
    height,
    cells: Array::make(size, fill),
    heights: Array::make(size, 0),
    max_height,
  }
}

///|
pub fn HeightGrid::in_bounds(self : HeightGrid, x : Int, y : Int) -> Bool {
  x >= 0 && x < self.width && y >= 0 && y < self.height
}

///|
pub fn HeightGrid::get(self : HeightGrid, x : Int, y : Int) -> Tile {
  self.cells[y * self.width + x]
}

///|
pub fn HeightGrid::set(
  self : HeightGrid,
  x : Int,
  y : Int,
  tile : Tile
) -> Unit {
  self.cells[y * self.width + x] = tile
}

///|
pub fn HeightGrid::get_height(self : HeightGrid, x : Int, y : Int) -> Int {
  self.heights[y * self.width + x]
}

///|
pub fn HeightGrid::set_height(
  self : HeightGrid,
  x : Int,
  y : Int,
  h : Int
) -> Unit {
  self.heights[y * self.width + x] = h
}

///|
pub fn HeightGrid::to_ascii(self : HeightGrid) -> String {
  let buf = StringBuilder::new()
  for y in 0..<self.height {
    for x in 0..<self.width {
      let tile = self.get(x, y)
      let h = self.get_height(x, y)
      if tile == Wall {
        buf.write_char('#')
      } else if h == 0 {
        buf.write_char('.')
      } else if h < 10 {
        buf.write_char((48 + h).unsafe_to_char()) // '0' + h
      } else {
        buf.write_char('+')
      }
    }
    if y < self.height - 1 {
      buf.write_char('\n')
    }
  }
  buf.to_string()
}

///| HeightGridをGrid2Dに変換（高さ情報は失われる）
pub fn HeightGrid::to_grid2d(self : HeightGrid) -> Grid2D {
  let grid = Grid2D::make(self.width, self.height, Wall)
  for y in 0..<self.height {
    for x in 0..<self.width {
      grid.set(x, y, self.get(x, y))
    }
  }
  grid
}
