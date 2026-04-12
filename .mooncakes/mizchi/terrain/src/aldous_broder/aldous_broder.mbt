///|
pub(all) struct AldousBroderConfig {
  width : Int
  height : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn AldousBroderConfig::default() -> AldousBroderConfig {
  {
    width: 41,
    height: 21,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct AldousBroderResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : AldousBroderConfig,
  rng : @types.RNG
) -> AldousBroderResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let total_cells = maze_w * maze_h
  // visited tracks which maze cells have been visited
  let visited = Array::make(total_cells, false)
  // Start at a random cell
  let start_col = rng.next_int(maze_w)
  let start_row = rng.next_int(maze_h)
  let mut cur_col = start_col
  let mut cur_row = start_row
  visited[cur_row * maze_w + cur_col] = true
  grid.set(cur_col * 2 + 1, cur_row * 2 + 1, @types.Corridor)
  let mut visited_count = 1
  // Directions in maze-cell coordinates
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  while visited_count < total_cells {
    // Pick a random cardinal direction
    let dir = rng.rand_pick(dirs)
    let next_col = cur_col + dir.0
    let next_row = cur_row + dir.1
    // Check bounds in maze coordinates
    if next_col >= 0 && next_col < maze_w && next_row >= 0 && next_row < maze_h {
      let next_idx = next_row * maze_w + next_col
      if not(visited[next_idx]) {
        // Mark as visited and carve passage
        visited[next_idx] = true
        visited_count += 1
        let cur_x = cur_col * 2 + 1
        let cur_y = cur_row * 2 + 1
        let next_x = next_col * 2 + 1
        let next_y = next_row * 2 + 1
        // Carve the neighbor cell
        grid.set(next_x, next_y, @types.Corridor)
        // Carve the wall between
        let wall_x = (cur_x + next_x) / 2
        let wall_y = (cur_y + next_y) / 2
        grid.set(wall_x, wall_y, @types.Corridor)
      }
      // Move to neighbor regardless
      cur_col = next_col
      cur_row = next_row
    }
    // If out of bounds, stay at current cell and try again
  }
  // Place rooms
  let rooms = place_rooms(grid, w, h, config, rng)
  // Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}

///|
fn place_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  config : AldousBroderConfig,
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
