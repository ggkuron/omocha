///|
pub(all) struct HexMazeConfig {
  cols : Int
  rows : Int
  room_chance : Double
}

///|
pub fn HexMazeConfig::default() -> HexMazeConfig {
  { cols: 20, rows: 15, room_chance: 0.15 }
}

///|
pub(all) struct HexMazeResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : HexMazeConfig, rng : @types.RNG) -> HexMazeResult {
  let grid = @types.HexGrid::make(config.cols, config.rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  let total = config.cols * config.rows
  let visited = Array::make(total, false)
  // Pick random start cell
  let start_col = rng.next_int(config.cols)
  let start_row = rng.next_int(config.rows)
  visited[start_row * config.cols + start_col] = true
  grid.set(start_col, start_row, @types.Floor)
  // Stack-based DFS
  let stack : Array[(Int, Int)] = [(start_col, start_row)]
  while stack.length() > 0 {
    let top = stack[stack.length() - 1]
    let col = top.0
    let row = top.1
    // Gather unvisited neighbors
    let neighbors = grid.neighbors6(col, row)
    let unvisited : Array[(Int, Int)] = []
    for n in neighbors {
      let nc = n.0
      let nr = n.1
      let idx = nr * config.cols + nc
      if not(visited[idx]) {
        unvisited.push((nc, nr))
      }
    }
    if unvisited.length() == 0 {
      // Backtrack
      let _popped = stack.pop()
    } else {
      // Pick random unvisited neighbor
      let next = rng.rand_pick(unvisited)
      let nc = next.0
      let nr = next.1
      let idx = nr * config.cols + nc
      visited[idx] = true
      grid.set(nc, nr, @types.Floor)
      stack.push((nc, nr))
      // Chance to create a room at this cell
      if rng.rand_bool(config.room_chance) {
        grid.set(nc, nr, @types.Floor)
        let rect : @types.Rect = { x: nc, y: nr, w: 1, h: 1 }
        let room : @types.Room = { rect, id: rooms.length() }
        rooms.push(room)
        // Also open up all neighbors of this cell to make a bigger room
        let room_neighbors = grid.neighbors6(nc, nr)
        for rn in room_neighbors {
          let rnc = rn.0
          let rnr = rn.1
          let rn_idx = rnr * config.cols + rnc
          if not(visited[rn_idx]) {
            visited[rn_idx] = true
            grid.set(rnc, rnr, @types.Floor)
          }
        }
      }
    }
  }
  // Place start and goal
  grid.set(start_col, start_row, @types.Start)
  // Find furthest visited cell from start for goal
  let mut goal_col = start_col
  let mut goal_row = start_row
  let mut max_dist = 0
  for r in 0..<config.rows {
    for c in 0..<config.cols {
      if visited[r * config.cols + c] {
        let dist = abs(c - start_col) + abs(r - start_row)
        if dist > max_dist {
          max_dist = dist
          goal_col = c
          goal_row = r
        }
      }
    }
  }
  grid.set(goal_col, goal_row, @types.Goal)
  { grid, rooms }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}
