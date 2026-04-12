///|
pub(all) struct SierpinskiConfig {
  width : Int
  height : Int
  depth : Int
  corridor_width : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn SierpinskiConfig::default() -> SierpinskiConfig {
  {
    width: 50,
    height: 25,
    depth: 3,
    corridor_width: 1,
    room_chance: 0.1,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct SierpinskiResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : SierpinskiConfig,
  rng : @types.RNG
) -> SierpinskiResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Recursively subdivide, carving the center sub-rectangle at each level
  sierpinski_recurse(grid, 1, 1, config.width - 2, config.height - 2, config.depth)
  // Connect adjacent floor areas with corridors
  connect_floors(grid, config.corridor_width)
  // Place rooms at random floor locations
  let mut room_id = 0
  for y in 2..<(config.height - 2) {
    for x in 2..<(config.width - 2) {
      if grid.get(x, y) != @types.Floor {
        continue
      }
      if not(rng.rand_bool(config.room_chance)) {
        continue
      }
      let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
      let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
      let rx = x - rw / 2
      let ry = y - rh / 2
      if rx < 1 || ry < 1 || rx + rw >= config.width - 1 || ry + rh >= config.height -
      1 {
        continue
      }
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for r in rooms {
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  { grid, rooms }
}

///|
fn sierpinski_recurse(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  w : Int,
  h : Int,
  depth : Int
) -> Unit {
  if depth <= 0 || w < 3 || h < 3 {
    return
  }
  // Divide into 3x3 sub-rectangles
  let sw = w / 3
  let sh = h / 3
  if sw < 1 || sh < 1 {
    return
  }
  // Carve the center sub-rectangle (row 1, col 1 in the 3x3 grid)
  let cx = x + sw
  let cy = y + sh
  for iy in cy..<(cy + sh) {
    for ix in cx..<(cx + sw) {
      if grid.in_bounds(ix, iy) {
        grid.set(ix, iy, @types.Floor)
      }
    }
  }
  // Recurse into the 8 surrounding sub-rectangles (skip center)
  for row in 0..<3 {
    for col in 0..<3 {
      if row == 1 && col == 1 {
        continue // skip center
      }
      let sub_x = x + col * sw
      let sub_y = y + row * sh
      sierpinski_recurse(grid, sub_x, sub_y, sw, sh, depth - 1)
    }
  }
}

///|
fn connect_floors(grid : @types.Grid2D, corridor_width : Int) -> Unit {
  // Scan for adjacent floor regions separated by a single wall line
  // Connect horizontally
  for y in 1..<(grid.height - 1) {
    for x in 1..<(grid.width - 2) {
      if grid.get(x, y) == @types.Floor && grid.get(x + 1, y) == @types.Wall &&
      x + 2 < grid.width && grid.get(x + 2, y) == @types.Floor {
        // Carve a corridor through the wall
        for w in 0..<corridor_width {
          let cy = y + w
          if grid.in_bounds(x + 1, cy) {
            grid.set(x + 1, cy, @types.Corridor)
          }
        }
      }
    }
  }
  // Connect vertically
  for x in 1..<(grid.width - 1) {
    for y in 1..<(grid.height - 2) {
      if grid.get(x, y) == @types.Floor && grid.get(x, y + 1) == @types.Wall &&
      y + 2 < grid.height && grid.get(x, y + 2) == @types.Floor {
        for w in 0..<corridor_width {
          let cx = x + w
          if grid.in_bounds(cx, y + 1) {
            grid.set(cx, y + 1, @types.Corridor)
          }
        }
      }
    }
  }
}
