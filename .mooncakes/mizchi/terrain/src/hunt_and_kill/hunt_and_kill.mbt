///|
pub(all) struct HuntAndKillConfig {
  width : Int
  height : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn HuntAndKillConfig::default() -> HuntAndKillConfig {
  {
    width: 41,
    height: 21,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct HuntAndKillResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : HuntAndKillConfig,
  rng : @types.RNG
) -> HuntAndKillResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  // visited tracks maze cells
  let visited = Array::make(maze_w * maze_h, false)
  // Start at random cell
  let mut cur_col = rng.next_int(maze_w)
  let mut cur_row = rng.next_int(maze_h)
  visited[cur_row * maze_w + cur_col] = true
  grid.set(cur_col * 2 + 1, cur_row * 2 + 1, @types.Corridor)
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  let mut done = false
  while not(done) {
    // Walk phase: try to move to a random unvisited neighbor
    let mut walked = true
    while walked {
      walked = false
      // Collect unvisited neighbors
      let neighbors : Array[(Int, Int)] = []
      for d in dirs {
        let nc = cur_col + d.0
        let nr = cur_row + d.1
        if nc >= 0 &&
          nc < maze_w &&
          nr >= 0 &&
          nr < maze_h &&
          not(visited[nr * maze_w + nc]) {
          neighbors.push((nc, nr))
        }
      }
      if neighbors.length() > 0 {
        let next = rng.rand_pick(neighbors)
        let next_col = next.0
        let next_row = next.1
        // Carve passage
        let cur_x = cur_col * 2 + 1
        let cur_y = cur_row * 2 + 1
        let next_x = next_col * 2 + 1
        let next_y = next_row * 2 + 1
        let wall_x = (cur_x + next_x) / 2
        let wall_y = (cur_y + next_y) / 2
        grid.set(wall_x, wall_y, @types.Corridor)
        grid.set(next_x, next_y, @types.Corridor)
        visited[next_row * maze_w + next_col] = true
        cur_col = next_col
        cur_row = next_row
        walked = true
      }
    }
    // Hunt phase: scan for first unvisited cell adjacent to a visited cell
    let mut found = false
    for row in 0..<maze_h {
      for col in 0..<maze_w {
        if visited[row * maze_w + col] {
          continue
        }
        // Check if this unvisited cell has a visited neighbor
        let visited_neighbors : Array[(Int, Int)] = []
        for d in dirs {
          let nc = col + d.0
          let nr = row + d.1
          if nc >= 0 &&
            nc < maze_w &&
            nr >= 0 &&
            nr < maze_h &&
            visited[nr * maze_w + nc] {
            visited_neighbors.push((nc, nr))
          }
        }
        if visited_neighbors.length() > 0 {
          // Connect to a random visited neighbor
          let neighbor = rng.rand_pick(visited_neighbors)
          let cell_x = col * 2 + 1
          let cell_y = row * 2 + 1
          let nb_x = neighbor.0 * 2 + 1
          let nb_y = neighbor.1 * 2 + 1
          let wall_x = (cell_x + nb_x) / 2
          let wall_y = (cell_y + nb_y) / 2
          grid.set(cell_x, cell_y, @types.Corridor)
          grid.set(wall_x, wall_y, @types.Corridor)
          visited[row * maze_w + col] = true
          cur_col = col
          cur_row = row
          found = true
          break
        }
      }
      if found {
        break
      }
    }
    if not(found) {
      done = true
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
  config : HuntAndKillConfig,
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
