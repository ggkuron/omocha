///|
pub(all) struct HexSnowflakeConfig {
  cols : Int
  rows : Int
  depth : Int
  branch_ratio : Double
}

///|
pub fn HexSnowflakeConfig::default() -> HexSnowflakeConfig {
  { cols: 30, rows: 25, depth: 4, branch_ratio: 0.6 }
}

///|
pub(all) struct HexSnowflakeResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn snowflake_branch(
  grid : @types.HexGrid,
  col : Int,
  row : Int,
  dir_idx : Int,
  length : Int,
  depth : Int,
  max_depth : Int,
  branch_ratio : Double,
  rng : @types.RNG
) -> Unit {
  if depth > max_depth || length <= 0 {
    return
  }
  // Direction offsets for 6 hex directions (odd-r offset)
  // We pre-compute direction vectors in cube space then convert
  // For simplicity, use the neighbors function and walk in chosen direction
  let mut c = col
  let mut r = row
  for _step in 0..<length {
    if not(grid.in_bounds(c, r)) {
      return
    }
    grid.set(c, r, @types.Floor)
    // Get neighbors and pick the one closest to our direction
    let neighbors = grid.neighbors6(c, r)
    if neighbors.length() == 0 {
      return
    }
    // Use dir_idx mod available neighbors as direction hint
    let ni = dir_idx % neighbors.length()
    let next = neighbors[ni]
    c = next.0
    r = next.1
  }
  if not(grid.in_bounds(c, r)) {
    return
  }
  grid.set(c, r, @types.Floor)
  // Branch in symmetric directions at reduced length
  let new_length = (length.to_double() * branch_ratio).to_int()
  if new_length <= 0 {
    return
  }
  // Branch into all 6 directions (creating symmetry)
  for d in 0..<6 {
    snowflake_branch(
      grid, c, r, d, new_length, depth + 1, max_depth, branch_ratio, rng,
    )
  }
}

///|
pub fn generate(
  config : HexSnowflakeConfig,
  rng : @types.RNG
) -> HexSnowflakeResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  let cx = cols / 2
  let cy = rows / 2
  // Place center crystal
  grid.set(cx, cy, @types.Floor)
  // Initial arm length based on grid size
  let arm_length = if cols < rows { cols / 4 } else { rows / 4 }
  // Grow 6 primary arms from center
  for d in 0..<6 {
    snowflake_branch(
      grid, cx, cy, d, arm_length, 0, config.depth, config.branch_ratio, rng,
    )
  }
  // Mark center as Start
  grid.set(cx, cy, @types.Start)
  // Build rooms from connected crystal regions using BFS
  let visited = Array::make(cols * rows, false)
  let mut room_id = 0
  for r in 0..<rows {
    for c in 0..<cols {
      let idx = r * cols + c
      if grid.get(c, r) != @types.Wall && not(visited[idx]) {
        // BFS to find connected region
        let mut min_c = c
        let mut max_c = c
        let mut min_r = r
        let mut max_r = r
        let queue : Array[(Int, Int)] = [(c, r)]
        visited[idx] = true
        let mut qi = 0
        while qi < queue.length() {
          let cur = queue[qi]
          qi += 1
          let neighbors = grid.neighbors6(cur.0, cur.1)
          for n in neighbors {
            let nidx = n.1 * cols + n.0
            if not(visited[nidx]) && grid.get(n.0, n.1) != @types.Wall {
              visited[nidx] = true
              queue.push(n)
              if n.0 < min_c {
                min_c = n.0
              }
              if n.0 > max_c {
                max_c = n.0
              }
              if n.1 < min_r {
                min_r = n.1
              }
              if n.1 > max_r {
                max_r = n.1
              }
            }
          }
        }
        rooms.push(
          {
            rect: { x: min_c, y: min_r, w: max_c - min_c + 1, h: max_r - min_r + 1 },
            id: room_id,
          },
        )
        room_id += 1
      }
    }
  }
  { grid, rooms }
}
