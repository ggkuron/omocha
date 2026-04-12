///|
pub(all) struct Grid2D {
  width : Int
  height : Int
  cells : Array[Tile]
}

///|
pub fn Grid2D::make(width : Int, height : Int, fill : Tile) -> Grid2D {
  { width, height, cells: Array::make(width * height, fill) }
}

///|
pub fn Grid2D::in_bounds(self : Grid2D, x : Int, y : Int) -> Bool {
  x >= 0 && x < self.width && y >= 0 && y < self.height
}

///|
pub fn Grid2D::get(self : Grid2D, x : Int, y : Int) -> Tile {
  self.cells[y * self.width + x]
}

///|
pub fn Grid2D::set(self : Grid2D, x : Int, y : Int, tile : Tile) -> Unit {
  self.cells[y * self.width + x] = tile
}

///|
pub fn Grid2D::fill_rect(
  self : Grid2D,
  rect : Rect,
  tile : Tile
) -> Unit {
  for y in rect.y..<(rect.y + rect.h) {
    for x in rect.x..<(rect.x + rect.w) {
      if self.in_bounds(x, y) {
        self.set(x, y, tile)
      }
    }
  }
}

///|
pub fn Grid2D::carve_line(
  self : Grid2D,
  from : Point,
  to : Point,
  tile : Tile
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if self.in_bounds(x, y) {
      self.set(x, y, tile)
    }
    if x != to.x {
      if x < to.x {
        x += 1
      } else {
        x -= 1
      }
    } else if y != to.y {
      if y < to.y {
        y += 1
      } else {
        y -= 1
      }
    }
  }
  if self.in_bounds(to.x, to.y) {
    self.set(to.x, to.y, tile)
  }
}

///|
pub fn Grid2D::carve_l_corridor(
  self : Grid2D,
  from : Point,
  to : Point,
  tile : Tile
) -> Unit {
  let mid : Point = { x: to.x, y: from.y }
  self.carve_line(from, mid, tile)
  self.carve_line(mid, to, tile)
}

///|
pub fn Grid2D::neighbors4(
  self : Grid2D,
  x : Int,
  y : Int
) -> Array[Point] {
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  let result : Array[Point] = []
  for d in dirs {
    let nx = x + d.0
    let ny = y + d.1
    if self.in_bounds(nx, ny) {
      result.push({ x: nx, y: ny })
    }
  }
  result
}

///|
pub fn Grid2D::count_neighbors(
  self : Grid2D,
  x : Int,
  y : Int,
  tile : Tile
) -> Int {
  let mut count = 0
  for dx in -1..=1 {
    for dy in -1..=1 {
      if dx == 0 && dy == 0 {
        continue
      }
      let nx = x + dx
      let ny = y + dy
      if self.in_bounds(nx, ny) && self.get(nx, ny) == tile {
        count += 1
      }
    }
  }
  count
}

///|
pub fn Grid2D::to_ascii(self : Grid2D) -> String {
  let buf = StringBuilder::new()
  for y in 0..<self.height {
    for x in 0..<self.width {
      buf.write_char(self.get(x, y).to_char())
    }
    if y < self.height - 1 {
      buf.write_char('\n')
    }
  }
  buf.to_string()
}

///|
pub fn Grid2D::clone(self : Grid2D) -> Grid2D {
  { width: self.width, height: self.height, cells: self.cells.copy() }
}
