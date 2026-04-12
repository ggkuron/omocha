///|
pub(all) struct HexBiomeConfig {
  cols : Int
  rows : Int
  num_regions : Int
  road_chance : Double
}

///|
pub fn HexBiomeConfig::default() -> HexBiomeConfig {
  { cols: 25, rows: 20, num_regions: 6, road_chance: 0.5 }
}

///|
pub(all) struct HexBiomeResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn biome_hex_dist(c1 : Int, r1 : Int, c2 : Int, r2 : Int) -> Int {
  let x1 = c1 - (r1 - (r1 & 1)) / 2
  let z1 = r1
  let y1 = -x1 - z1
  let x2 = c2 - (r2 - (r2 & 1)) / 2
  let z2 = r2
  let y2 = -x2 - z2
  let dx = if x1 > x2 { x1 - x2 } else { x2 - x1 }
  let dy = if y1 > y2 { y1 - y2 } else { y2 - y1 }
  let dz = if z1 > z2 { z1 - z2 } else { z2 - z1 }
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
pub fn generate(
  config : HexBiomeConfig,
  rng : @types.RNG
) -> HexBiomeResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Place seed points for Voronoi regions
  let seeds : Array[(Int, Int)] = []
  let biome_tiles : Array[@types.Tile] = [
    @types.Floor, // plains
    @types.Wall, // mountains
    @types.Water, // lakes
    @types.Floor,
    @types.Water,
    @types.Wall,
    @types.Floor,
    @types.Water,
  ]
  let num_regions = config.num_regions
  for _i in 0..<num_regions {
    let mut attempts = 0
    while attempts < 100 {
      let c = rng.rand_range(1, cols - 1)
      let r = rng.rand_range(1, rows - 1)
      let mut too_close = false
      for s in seeds {
        if biome_hex_dist(c, r, s.0, s.1) < 3 {
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
  let actual = seeds.length()
  if actual == 0 {
    return { grid, rooms }
  }
  // Assign biome type to each seed
  let region_biome : Array[@types.Tile] = []
  for i in 0..<actual {
    region_biome.push(biome_tiles[i % biome_tiles.length()])
  }
  // Assign each cell to nearest seed (Voronoi)
  let region_map = Array::make(cols * rows, -1)
  for r in 0..<rows {
    for c in 0..<cols {
      let mut best_dist = cols + rows
      let mut best_id = 0
      for i in 0..<actual {
        let dist = biome_hex_dist(c, r, seeds[i].0, seeds[i].1)
        if dist < best_dist {
          best_dist = dist
          best_id = i
        }
      }
      region_map[r * cols + c] = best_id
      grid.set(c, r, region_biome[best_id])
    }
  }
  // Build roads between adjacent regions
  // First find region adjacencies
  let adjacency : Array[(Int, Int)] = []
  for r in 0..<rows {
    for c in 0..<cols {
      let my_region = region_map[r * cols + c]
      let neighbors = grid.neighbors6(c, r)
      for n in neighbors {
        let nr = region_map[n.1 * cols + n.0]
        if nr != my_region {
          // Record adjacency (ordered pair)
          let a = if my_region < nr { my_region } else { nr }
          let b = if my_region < nr { nr } else { my_region }
          let mut found = false
          for adj in adjacency {
            if adj.0 == a && adj.1 == b {
              found = true
              break
            }
          }
          if not(found) {
            adjacency.push((a, b))
          }
        }
      }
    }
  }
  // Draw roads between adjacent region centers
  for adj in adjacency {
    if not(rng.rand_bool(config.road_chance)) {
      continue
    }
    let s1 = seeds[adj.0]
    let s2 = seeds[adj.1]
    // Bresenham-style line between seed centers
    let mut c = s1.0
    let mut r = s1.1
    let max_steps = cols + rows
    let mut steps = 0
    while (c != s2.0 || r != s2.1) && steps < max_steps {
      steps += 1
      grid.set(c, r, @types.Corridor)
      // Move towards target
      if c != s2.0 {
        if c < s2.0 {
          c += 1
        } else {
          c -= 1
        }
      } else if r != s2.1 {
        if r < s2.1 {
          r += 1
        } else {
          r -= 1
        }
      }
    }
    if grid.in_bounds(s2.0, s2.1) {
      grid.set(s2.0, s2.1, @types.Corridor)
    }
  }
  // Build room bounding boxes for each region
  let region_min_c = Array::make(actual, cols)
  let region_min_r = Array::make(actual, rows)
  let region_max_c = Array::make(actual, 0)
  let region_max_r = Array::make(actual, 0)
  let region_has_cells = Array::make(actual, false)
  for r in 0..<rows {
    for c in 0..<cols {
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
  for i in 0..<actual {
    if region_has_cells[i] {
      rooms.push(
        {
          rect: {
            x: region_min_c[i],
            y: region_min_r[i],
            w: region_max_c[i] - region_min_c[i] + 1,
            h: region_max_r[i] - region_min_r[i] + 1,
          },
          id: i,
        },
      )
    }
  }
  { grid, rooms }
}
