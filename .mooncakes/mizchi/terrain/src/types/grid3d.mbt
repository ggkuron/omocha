///| Grid3D — 多層グリッド
pub(all) struct Grid3D {
  width : Int
  height : Int
  depth : Int
  layers : Array[Grid2D]
}

///|
pub fn Grid3D::make(
  width : Int,
  height : Int,
  depth : Int,
  fill : Tile
) -> Grid3D {
  let layers : Array[Grid2D] = []
  for _i in 0..<depth {
    layers.push(Grid2D::make(width, height, fill))
  }
  { width, height, depth, layers }
}

///|
pub fn Grid3D::get_layer(self : Grid3D, z : Int) -> Grid2D {
  self.layers[z]
}

///|
pub fn Grid3D::get(self : Grid3D, x : Int, y : Int, z : Int) -> Tile {
  self.layers[z].get(x, y)
}

///|
pub fn Grid3D::set(
  self : Grid3D,
  x : Int,
  y : Int,
  z : Int,
  tile : Tile
) -> Unit {
  self.layers[z].set(x, y, tile)
}

///| 6方向隣接: 上下左右 + 上層/下層
pub fn Grid3D::neighbors6(
  self : Grid3D,
  x : Int,
  y : Int,
  z : Int
) -> Array[(Int, Int, Int)] {
  let result : Array[(Int, Int, Int)] = []
  let dirs : Array[(Int, Int, Int)] = [
    (-1, 0, 0),
    (1, 0, 0),
    (0, -1, 0),
    (0, 1, 0),
    (0, 0, -1),
    (0, 0, 1),
  ]
  for d in dirs {
    let nx = x + d.0
    let ny = y + d.1
    let nz = z + d.2
    if nx >= 0 &&
      nx < self.width &&
      ny >= 0 &&
      ny < self.height &&
      nz >= 0 &&
      nz < self.depth {
      result.push((nx, ny, nz))
    }
  }
  result
}

///|
pub fn Grid3D::to_ascii(self : Grid3D, layer_idx : Int) -> String {
  self.layers[layer_idx].to_ascii()
}
