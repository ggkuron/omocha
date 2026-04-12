///|
pub(all) struct GrowingTreeConfig {
  width : Int
  height : Int
  newest_weight : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn GrowingTreeConfig::default() -> GrowingTreeConfig {
  {
    width: 41,
    height: 21,
    newest_weight: 1.0,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct GrowingTreeResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : GrowingTreeConfig,
  rng : @types.RNG
) -> GrowingTreeResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Start with a random cell at odd coordinates
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let start_col = rng.next_int(maze_w)
  let start_row = rng.next_int(maze_h)
  let start_x = start_col * 2 + 1
  let start_y = start_row * 2 + 1
  grid.set(start_x, start_y, @types.Corridor)
  let cells : Array[@types.Point] = [{ x: start_x, y: start_y }]
  while cells.length() > 0 {
    // Pick cell: blend between newest (DFS) and random (Prim's)
    let index = if rng.rand_bool(config.newest_weight) {
      cells.length() - 1
    } else {
      rng.next_int(cells.length())
    }
    let current = cells[index]
    let neighbors = get_unvisited_neighbors(grid, current.x, current.y, w, h)
    if neighbors.length() == 0 {
      // Remove this cell from the list
      let last = cells.length() - 1
      cells[index] = cells[last]
      ignore(cells.pop())
    } else {
      let next = rng.rand_pick(neighbors)
      // Carve wall between current and next
      let wall_x = current.x + (next.x - current.x) / 2
      let wall_y = current.y + (next.y - current.y) / 2
      grid.set(wall_x, wall_y, @types.Corridor)
      grid.set(next.x, next.y, @types.Corridor)
      cells.push(next)
    }
  }
  // Place rooms
  let rooms = place_rooms(grid, w, h, config, rng)
  // Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}

///|
fn get_unvisited_neighbors(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  w : Int,
  h : Int
) -> Array[@types.Point] {
  let dirs : Array[(Int, Int)] = [(0, -2), (0, 2), (-2, 0), (2, 0)]
  let result : Array[@types.Point] = []
  for d in dirs {
    let nx = x + d.0
    let ny = y + d.1
    if nx > 0 && nx < w - 1 && ny > 0 && ny < h - 1 &&
      grid.get(nx, ny) == @types.Wall {
      result.push({ x: nx, y: ny })
    }
  }
  result
}

///|
fn place_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  config : GrowingTreeConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for _ in 0..<(maze_w * maze_h) {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(1, w - rw - 1)
    let ry = rng.rand_range(1, h - rh - 1)
    if rx + rw < w && ry + rh < h {
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
  rooms
}
