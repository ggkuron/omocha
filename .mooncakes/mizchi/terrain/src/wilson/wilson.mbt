///|
pub(all) struct WilsonConfig {
  width : Int
  height : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn WilsonConfig::default() -> WilsonConfig {
  {
    width: 41,
    height: 21,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct WilsonResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : WilsonConfig, rng : @types.RNG) -> WilsonResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let total_cells = maze_w * maze_h
  // in_maze tracks which maze cells (by index) are part of the maze
  let in_maze = Array::make(total_cells, false)
  // Pick a random cell to start as "in maze"
  let first = rng.next_int(total_cells)
  in_maze[first] = true
  let first_x = (first % maze_w) * 2 + 1
  let first_y = (first / maze_w) * 2 + 1
  grid.set(first_x, first_y, @types.Corridor)
  let mut remaining = total_cells - 1
  while remaining > 0 {
    // Pick a random cell NOT in maze
    let mut start_idx = rng.next_int(total_cells)
    while in_maze[start_idx] {
      start_idx = (start_idx + 1) % total_cells
    }
    // Perform loop-erased random walk
    // path stores cell indices in order of the walk
    let path : Array[Int] = [start_idx]
    // position_in_path maps cell index -> position in path (for loop erasure)
    let position_in_path : Map[Int, Int] = {}
    position_in_path[start_idx] = 0
    let mut current = start_idx
    // Walk until we hit a cell that is in the maze
    while not(in_maze[current]) {
      let cx = current % maze_w
      let cy = current / maze_w
      // Get valid neighbor cells (in maze coordinates)
      let neighbors : Array[Int] = []
      if cx > 0 {
        neighbors.push(cy * maze_w + (cx - 1))
      }
      if cx < maze_w - 1 {
        neighbors.push(cy * maze_w + (cx + 1))
      }
      if cy > 0 {
        neighbors.push((cy - 1) * maze_w + cx)
      }
      if cy < maze_h - 1 {
        neighbors.push((cy + 1) * maze_w + cx)
      }
      let next = rng.rand_pick(neighbors)
      match position_in_path.get(next) {
        Some(pos) => {
          // Loop detected: erase from pos+1 to end
          let erase_start = pos + 1
          let path_len = path.length()
          for i in erase_start..<path_len {
            ignore(position_in_path.remove(path[i]))
          }
          // Truncate path to pos+1 elements
          let keep = pos + 1
          while path.length() > keep {
            ignore(path.pop())
          }
          current = path[path.length() - 1]
        }
        None => {
          path.push(next)
          position_in_path[next] = path.length() - 1
          current = next
        }
      }
    }
    // current is in_maze; carve the path into the grid
    for i in 0..<path.length() {
      let cell_idx = path[i]
      let cell_x = (cell_idx % maze_w) * 2 + 1
      let cell_y = (cell_idx / maze_w) * 2 + 1
      grid.set(cell_x, cell_y, @types.Corridor)
      in_maze[cell_idx] = true
      remaining -= 1
      // Carve wall between this cell and the next in path
      if i + 1 < path.length() {
        let next_idx = path[i + 1]
        let next_x = (next_idx % maze_w) * 2 + 1
        let next_y = (next_idx / maze_w) * 2 + 1
        let wall_x = (cell_x + next_x) / 2
        let wall_y = (cell_y + next_y) / 2
        grid.set(wall_x, wall_y, @types.Corridor)
      } else {
        // Last cell in path connects to current (which is already in maze)
        // current is already the cell we walked into that was in_maze
        // We need to carve the wall between this cell and the in-maze cell
        let cur_x = (current % maze_w) * 2 + 1
        let cur_y = (current / maze_w) * 2 + 1
        let wall_x = (cell_x + cur_x) / 2
        let wall_y = (cell_y + cur_y) / 2
        grid.set(wall_x, wall_y, @types.Corridor)
      }
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
fn place_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  config : WilsonConfig,
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
