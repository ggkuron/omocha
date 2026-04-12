///|
pub(all) struct HexVoronoiConfig {
  cols : Int
  rows : Int
  num_seeds : Int
  shrink : Int
}

///|
pub fn HexVoronoiConfig::default() -> HexVoronoiConfig {
  { cols: 30, rows: 20, num_seeds: 8, shrink: 1 }
}

///|
pub(all) struct HexVoronoiResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn hex_distance(c1 : Int, r1 : Int, c2 : Int, r2 : Int) -> Int {
  // Convert odd-r offset coordinates to cube coordinates for distance calc
  let x1 = c1 - (r1 - (r1 & 1)) / 2
  let z1 = r1
  let y1 = -x1 - z1
  let x2 = c2 - (r2 - (r2 & 1)) / 2
  let z2 = r2
  let y2 = -x2 - z2
  let dx = if x1 > x2 { x1 - x2 } else { x2 - x1 }
  let dy = if y1 > y2 { y1 - y2 } else { y2 - y1 }
  let dz = if z1 > z2 { z1 - z2 } else { z2 - z1 }
  // Max of absolute differences in cube coords = hex distance
  let mut m = dx
  if dy > m {
    m = dy
  }
  if dz > m {
    m = dz
  }
  m
}

///|
pub fn generate(config : HexVoronoiConfig, rng : @types.RNG) -> HexVoronoiResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Place random seed points
  let seeds : Array[(Int, Int)] = []
  let num_seeds = if config.num_seeds > cols * rows {
    cols * rows
  } else {
    config.num_seeds
  }
  for _i in 0..<num_seeds {
    let mut attempts = 0
    while attempts < 100 {
      let c = rng.next_int(cols)
      let r = rng.next_int(rows)
      // Check minimum distance from existing seeds
      let mut too_close = false
      for s in seeds {
        if hex_distance(c, r, s.0, s.1) < 3 {
          too_close = true
          break
        }
      }
      if not(too_close) {
        seeds.push((c, r))
        break
      }
      attempts += 1
    }
  }
  let actual_seeds = seeds.length()
  if actual_seeds == 0 {
    return { grid, rooms }
  }
  // Assign each cell to nearest seed
  let region_map = Array::make(cols * rows, -1)
  for r in 0..<rows {
    for c in 0..<cols {
      let mut best_dist = cols + rows
      let mut best_id = 0
      for i in 0..<actual_seeds {
        let dist = hex_distance(c, r, seeds[i].0, seeds[i].1)
        if dist < best_dist {
          best_dist = dist
          best_id = i
        }
      }
      region_map[r * cols + c] = best_id
    }
  }
  // Initial fill: set all cells to Floor
  for r in 0..<rows {
    for c in 0..<cols {
      grid.set(c, r, @types.Floor)
    }
  }
  // Shrink: set border cells (where neighbor has different region) to Wall
  for _shrink_pass in 0..<config.shrink {
    let to_wall : Array[(Int, Int)] = []
    for r in 0..<rows {
      for c in 0..<cols {
        if grid.get(c, r) == @types.Wall {
          continue
        }
        // Border of map
        if c == 0 || r == 0 || c == cols - 1 || r == rows - 1 {
          to_wall.push((c, r))
          continue
        }
        let my_region = region_map[r * cols + c]
        let neighbors = grid.neighbors6(c, r)
        for n in neighbors {
          let nr = region_map[n.1 * cols + n.0]
          if nr != my_region {
            to_wall.push((c, r))
            break
          }
        }
      }
    }
    for cell in to_wall {
      grid.set(cell.0, cell.1, @types.Wall)
    }
  }
  // Build rooms from remaining regions
  let region_min_c = Array::make(actual_seeds, cols)
  let region_min_r = Array::make(actual_seeds, rows)
  let region_max_c = Array::make(actual_seeds, 0)
  let region_max_r = Array::make(actual_seeds, 0)
  let region_has_cells = Array::make(actual_seeds, false)
  for r in 0..<rows {
    for c in 0..<cols {
      if grid.get(c, r) != @types.Wall {
        let rid = region_map[r * cols + c]
        region_has_cells[rid] = true
        if c < region_min_c[rid] {
          region_min_c[rid] = c
        }
        if c > region_max_c[rid] {
          region_max_c[rid] = c
        }
        if r < region_min_r[rid] {
          region_min_r[rid] = r
        }
        if r > region_max_r[rid] {
          region_max_r[rid] = r
        }
      }
    }
  }
  for i in 0..<actual_seeds {
    if region_has_cells[i] {
      let rect : @types.Rect = {
        x: region_min_c[i],
        y: region_min_r[i],
        w: region_max_c[i] - region_min_c[i] + 1,
        h: region_max_r[i] - region_min_r[i] + 1,
      }
      rooms.push({ rect, id: rooms.length() })
    }
  }
  // Place doors between adjacent regions
  for r in 0..<rows {
    for c in 0..<cols {
      if grid.get(c, r) != @types.Wall {
        continue
      }
      // Check if this wall cell borders exactly 2 different floor regions
      let neighbors = grid.neighbors6(c, r)
      let adj_regions : Array[Int] = []
      for n in neighbors {
        if grid.get(n.0, n.1) != @types.Wall {
          let rid = region_map[n.1 * cols + n.0]
          let mut found = false
          for ar in adj_regions {
            if ar == rid {
              found = true
              break
            }
          }
          if not(found) {
            adj_regions.push(rid)
          }
        }
      }
      if adj_regions.length() >= 2 {
        grid.set(c, r, @types.Door)
      }
    }
  }
  { grid, rooms }
}
