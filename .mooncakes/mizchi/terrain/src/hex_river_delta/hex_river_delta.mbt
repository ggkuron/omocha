///|
pub(all) struct HexRiverDeltaConfig {
  cols : Int
  rows : Int
  num_sources : Int
  branch_chance : Double
}

///|
pub fn HexRiverDeltaConfig::default() -> HexRiverDeltaConfig {
  { cols: 25, rows: 20, num_sources: 3, branch_chance: 0.4 }
}

///|
pub(all) struct HexRiverDeltaResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn grow_river(
  grid : @types.HexGrid,
  col : Int,
  row : Int,
  rng : @types.RNG,
  branch_chance : Double,
  depth : Int,
  max_depth : Int,
  visited : Array[Bool]
) -> Unit {
  if depth >= max_depth {
    return
  }
  if not(grid.in_bounds(col, row)) {
    return
  }
  let idx = row * grid.cols + col
  if visited[idx] {
    return
  }
  visited[idx] = true
  grid.set(col, row, @types.Water)
  // Get neighbors and shuffle for randomness
  let neighbors = grid.neighbors6(col, row)
  rng.rand_shuffle(neighbors)
  // Prefer outward direction (away from center)
  let cx = grid.cols / 2
  let cy = grid.rows / 2
  let mut primary_picked = false
  for n in neighbors {
    let nc = n.0
    let nr = n.1
    let nidx = nr * grid.cols + nc
    if visited[nidx] {
      continue
    }
    // Calculate distance from center for directional preference
    let old_dist = abs_int(col - cx) + abs_int(row - cy)
    let new_dist = abs_int(nc - cx) + abs_int(nr - cy)
    if not(primary_picked) && new_dist >= old_dist {
      // Primary channel: always extend outward
      primary_picked = true
      grow_river(
        grid, nc, nr, rng, branch_chance, depth + 1, max_depth, visited,
      )
    } else if rng.rand_bool(branch_chance * (1.0 - depth.to_double() / max_depth.to_double())) {
      // Branch with decreasing probability
      grow_river(
        grid,
        nc,
        nr,
        rng,
        branch_chance * 0.7,
        depth + 1,
        max_depth,
        visited,
      )
    }
  }
  // If we didn't pick a primary, try any unvisited neighbor
  if not(primary_picked) {
    for n in neighbors {
      let nidx = n.1 * grid.cols + n.0
      if not(visited[nidx]) {
        grow_river(
          grid, n.0, n.1, rng, branch_chance, depth + 1, max_depth, visited,
        )
        break
      }
    }
  }
}

///|
fn abs_int(x : Int) -> Int {
  if x < 0 {
    -x
  } else {
    x
  }
}

///|
pub fn generate(
  config : HexRiverDeltaConfig,
  rng : @types.RNG
) -> HexRiverDeltaResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Floor)
  let rooms : Array[@types.Room] = []
  let visited = Array::make(cols * rows, false)
  let cx = cols / 2
  let cy = rows / 2
  let max_depth = cols + rows
  // Create river sources near center
  for i in 0..<config.num_sources {
    let sc = cx + rng.rand_range(-2, 3)
    let sr = cy + rng.rand_range(-2, 3)
    if grid.in_bounds(sc, sr) {
      grow_river(
        grid, sc, sr, rng, config.branch_chance, 0, max_depth, visited,
      )
    }
    // Track source as a room
    if grid.in_bounds(sc, sr) {
      rooms.push(
        { rect: { x: sc, y: sr, w: 1, h: 1 }, id: i },
      )
    }
  }
  // Build land region rooms from connected Floor areas
  let region_visited = Array::make(cols * rows, false)
  let mut room_id = rooms.length()
  for r in 0..<rows {
    for c in 0..<cols {
      let idx = r * cols + c
      if grid.get(c, r) == @types.Floor && not(region_visited[idx]) {
        // BFS to find land region bounds
        let mut min_c = c
        let mut max_c = c
        let mut min_r = r
        let mut max_r = r
        let queue : Array[(Int, Int)] = [(c, r)]
        region_visited[idx] = true
        let mut qi = 0
        while qi < queue.length() {
          let cur = queue[qi]
          qi += 1
          let neighbors = grid.neighbors6(cur.0, cur.1)
          for n in neighbors {
            let nidx = n.1 * cols + n.0
            if not(region_visited[nidx]) && grid.get(n.0, n.1) == @types.Floor {
              region_visited[nidx] = true
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
        if queue.length() >= 3 {
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
  }
  { grid, rooms }
}
