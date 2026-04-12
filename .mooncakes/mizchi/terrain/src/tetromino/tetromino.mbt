///|
pub(all) struct TetrominoConfig {
  width : Int
  height : Int
  max_attempts : Int
  connect_adjacent : Bool
  corridor_width : Int
}

///|
pub fn TetrominoConfig::default() -> TetrominoConfig {
  { width: 50, height: 25, max_attempts: 200, connect_adjacent: true, corridor_width: 1 }
}

///|
pub(all) struct TetrominoResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

// Each tetromino shape is defined as an array of (dx, dy) offsets from origin.
// Shapes are in a standard 4-cell configuration.

///|
fn get_tetromino_shapes() -> Array[Array[(Int, Int)]] {
  [
    // I
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    // O
    [(0, 0), (1, 0), (0, 1), (1, 1)],
    // T
    [(0, 0), (1, 0), (2, 0), (1, 1)],
    // S
    [(1, 0), (2, 0), (0, 1), (1, 1)],
    // Z
    [(0, 0), (1, 0), (1, 1), (2, 1)],
    // L
    [(0, 0), (0, 1), (0, 2), (1, 2)],
    // J
    [(1, 0), (1, 1), (1, 2), (0, 2)],
  ]
}

///|
fn rotate_shape(shape : Array[(Int, Int)]) -> Array[(Int, Int)] {
  let rotated : Array[(Int, Int)] = []
  for cell in shape {
    // 90-degree clockwise rotation: (x, y) -> (y, -x)
    rotated.push((cell.1, -cell.0))
  }
  // Normalize to non-negative coordinates
  let mut min_x = 0
  let mut min_y = 0
  for cell in rotated {
    if cell.0 < min_x {
      min_x = cell.0
    }
    if cell.1 < min_y {
      min_y = cell.1
    }
  }
  let result : Array[(Int, Int)] = []
  for cell in rotated {
    result.push((cell.0 - min_x, cell.1 - min_y))
  }
  result
}

///|
fn shape_bounds(shape : Array[(Int, Int)]) -> (Int, Int) {
  let mut max_x = 0
  let mut max_y = 0
  for cell in shape {
    if cell.0 > max_x {
      max_x = cell.0
    }
    if cell.1 > max_y {
      max_y = cell.1
    }
  }
  (max_x + 1, max_y + 1)
}

// Scale factor: each tetromino cell becomes a scale x scale block
///|
let tetromino_scale : Int = 3

///|
fn can_place_tetromino(
  grid : @types.Grid2D,
  shape : Array[(Int, Int)],
  ox : Int,
  oy : Int
) -> Bool {
  let scale = tetromino_scale
  for cell in shape {
    let bx = ox + cell.0 * scale
    let by = oy + cell.1 * scale
    for dy in 0..<scale {
      for dx in 0..<scale {
        let px = bx + dx
        let py = by + dy
        if not(grid.in_bounds(px, py)) {
          return false
        }
        if grid.get(px, py) != @types.Wall {
          return false
        }
      }
    }
  }
  true
}

///|
fn place_tetromino(
  grid : @types.Grid2D,
  shape : Array[(Int, Int)],
  ox : Int,
  oy : Int
) -> @types.Rect {
  let scale = tetromino_scale
  let mut min_x = ox + shape[0].0 * scale
  let mut min_y = oy + shape[0].1 * scale
  let mut max_x = min_x
  let mut max_y = min_y
  for cell in shape {
    let bx = ox + cell.0 * scale
    let by = oy + cell.1 * scale
    for dy in 0..<scale {
      for dx in 0..<scale {
        grid.set(bx + dx, by + dy, @types.Floor)
      }
    }
    if bx < min_x {
      min_x = bx
    }
    if by < min_y {
      min_y = by
    }
    if bx + scale > max_x {
      max_x = bx + scale
    }
    if by + scale > max_y {
      max_y = by + scale
    }
  }
  { x: min_x, y: min_y, w: max_x - min_x, h: max_y - min_y }
}

///|
fn rooms_adjacent(r1 : @types.Rect, r2 : @types.Rect) -> Bool {
  // Two rooms are adjacent if their bounding rects are within 2 tiles
  let gap = 2
  let h_overlap = r1.x < r2.x + r2.w + gap && r1.x + r1.w + gap > r2.x
  let v_overlap = r1.y < r2.y + r2.h + gap && r1.y + r1.h + gap > r2.y
  if not(h_overlap) || not(v_overlap) {
    return false
  }
  // But they must not fully overlap (they are separate placements)
  let fully_separated_h = r1.x + r1.w <= r2.x || r2.x + r2.w <= r1.x
  let fully_separated_v = r1.y + r1.h <= r2.y || r2.y + r2.h <= r1.y
  fully_separated_h || fully_separated_v
}

///|
pub fn generate(config : TetrominoConfig, rng : @types.RNG) -> TetrominoResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let shapes = get_tetromino_shapes()
  let rects : Array[@types.Rect] = []
  for _attempt in 0..<config.max_attempts {
    // Pick a random shape
    let mut shape = shapes[rng.next_int(shapes.length())].copy()
    // Random rotation (0-3 times)
    let rotations = rng.next_int(4)
    for _r in 0..<rotations {
      shape = rotate_shape(shape)
    }
    let bounds = shape_bounds(shape)
    let scale = tetromino_scale
    let max_ox = config.width - bounds.0 * scale
    let max_oy = config.height - bounds.1 * scale
    if max_ox <= 1 || max_oy <= 1 {
      continue
    }
    let ox = rng.rand_range(1, max_ox)
    let oy = rng.rand_range(1, max_oy)
    if can_place_tetromino(grid, shape, ox, oy) {
      let rect = place_tetromino(grid, shape, ox, oy)
      let room : @types.Room = { rect, id: rooms.length() }
      rooms.push(room)
      rects.push(rect)
    }
  }
  // Connect adjacent tetromino rooms with corridors
  if config.connect_adjacent {
    for i in 0..<rects.length() {
      for j = i + 1; j < rects.length(); {
        if rooms_adjacent(rects[i], rects[j]) {
          let c1 = rects[i].center()
          let c2 = rects[j].center()
          // Carve corridor with specified width
          for w in 0..<config.corridor_width {
            let p1 : @types.Point = { x: c1.x, y: c1.y + w }
            let p2 : @types.Point = { x: c2.x, y: c2.y + w }
            grid.carve_l_corridor(p1, p2, @types.Corridor)
          }
        }
        continue j + 1
      }
    }
  }
  { grid, rooms }
}
