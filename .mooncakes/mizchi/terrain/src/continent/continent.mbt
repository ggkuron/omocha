///|
pub(all) struct ContinentConfig {
  width : Int
  height : Int
  land_ratio : Double
  num_biomes : Int
}

///|
pub fn ContinentConfig::default() -> ContinentConfig {
  { width: 60, height: 40, land_ratio: 0.5, num_biomes: 4 }
}

///|
pub(all) struct ContinentResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : ContinentConfig,
  rng : @types.RNG
) -> ContinentResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Water)
  let cx = w / 2
  let cy = h / 2
  // Maximum radius for the land mass based on land_ratio
  let max_dim = if w < h { w } else { h }
  let base_radius = (max_dim.to_double() * config.land_ratio * 0.5)
  // Generate land mass using distance from center with noise
  for y in 0..<h {
    for x in 0..<w {
      let dx = (x - cx).to_double()
      let dy = (y - cy).to_double()
      let dist = sqrt(dx * dx + dy * dy)
      // Add noise based on angle and position
      let noise = rng.rand_range(-3, 4).to_double()
      let threshold = base_radius + noise
      if dist < threshold {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Smooth the coastline with cellular automata
  for _ in 0..<3 {
    let old = grid.clone()
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let land_count = count_land(old, x, y)
        if old.get(x, y) != @types.Water {
          if land_count < 3 {
            grid.set(x, y, @types.Water)
          }
        } else if land_count >= 6 {
          grid.set(x, y, @types.Floor)
        }
      }
    }
  }
  // Apply concentric biome rings based on distance from center
  // Ring order from outside to inside: beach (Corridor), forest (Wall), mountains (Door)
  if config.num_biomes >= 2 {
    let ring_step = base_radius / config.num_biomes.to_double()
    for y in 0..<h {
      for x in 0..<w {
        if grid.get(x, y) == @types.Water {
          continue
        }
        let dx = (x - cx).to_double()
        let dy = (y - cy).to_double()
        let dist = sqrt(dx * dx + dy * dy)
        // Determine which ring this cell belongs to
        let ring = ((base_radius - dist) / ring_step).to_int()
        if ring <= 0 {
          // Outermost ring: beach
          grid.set(x, y, @types.Corridor)
        } else if ring >= config.num_biomes - 1 {
          // Innermost ring: mountains
          grid.set(x, y, @types.Door)
        } else if ring >= config.num_biomes / 2 {
          // Inner rings: forest (dense Wall patterns)
          if rng.rand_bool(0.4) {
            grid.set(x, y, @types.Wall)
          }
          // else stays Floor (clearings in forest)
        }
        // Middle rings stay as Floor (plains)
      }
    }
  }
  // Add some random terrain features within biomes
  for _ in 0..<(w * h / 40) {
    let x = rng.rand_range(2, w - 2)
    let y = rng.rand_range(2, h - 2)
    if grid.get(x, y) == @types.Floor {
      // Scatter some trees in plains
      if rng.rand_bool(0.15) {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Place Start and Goal on the landmass
  let start_pos = find_tile_near(grid, 2, cy, @types.Floor, w, h)
  grid.set(start_pos.x, start_pos.y, @types.Start)
  let goal_pos = find_tile_near(grid, w - 3, cy, @types.Floor, w, h)
  if goal_pos.x != start_pos.x || goal_pos.y != start_pos.y {
    grid.set(goal_pos.x, goal_pos.y, @types.Goal)
  }
  // Find rooms (contiguous land regions)
  let rooms = find_land_rooms(grid, w, h)
  { grid, rooms }
}

///|
fn sqrt(x : Double) -> Double {
  if x <= 0.0 {
    return 0.0
  }
  let mut guess = x / 2.0
  for _ in 0..<20 {
    guess = (guess + x / guess) / 2.0
  }
  guess
}

///|
fn count_land(grid : @types.Grid2D, x : Int, y : Int) -> Int {
  let mut count = 0
  for dy in -1..=1 {
    for dx in -1..=1 {
      if dx == 0 && dy == 0 {
        continue
      }
      let nx = x + dx
      let ny = y + dy
      if grid.in_bounds(nx, ny) && grid.get(nx, ny) != @types.Water {
        count += 1
      }
    }
  }
  count
}

///|
fn find_tile_near(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int,
  tile : @types.Tile,
  _w : Int,
  _h : Int
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let adx = if dx < 0 { -dx } else { dx }
        let ady = if dy < 0 { -dy } else { dy }
        if adx + ady != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && grid.get(x, y) == tile {
          return { x, y }
        }
      }
    }
  }
  { x: cx, y: cy }
}

///|
fn find_land_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int
) -> Array[@types.Room] {
  let visited = Array::make(w * h, false)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for y in 0..<h {
    for x in 0..<w {
      let idx = y * w + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        // Flood fill to find contiguous region
        let mut min_x = w
        let mut min_y = h
        let mut max_x = 0
        let mut max_y = 0
        let mut count = 0
        let stack : Array[@types.Point] = [{ x, y }]
        visited[idx] = true
        while stack.length() > 0 {
          let p = stack.pop().unwrap()
          count += 1
          if p.x < min_x {
            min_x = p.x
          }
          if p.y < min_y {
            min_y = p.y
          }
          if p.x > max_x {
            max_x = p.x
          }
          if p.y > max_y {
            max_y = p.y
          }
          let neighbors = grid.neighbors4(p.x, p.y)
          for n in neighbors {
            let ni = n.y * w + n.x
            if not(visited[ni]) && grid.get(n.x, n.y) == @types.Floor {
              visited[ni] = true
              stack.push(n)
            }
          }
        }
        if count > 4 {
          rooms.push(
            {
              rect: {
                x: min_x,
                y: min_y,
                w: max_x - min_x + 1,
                h: max_y - min_y + 1,
              },
              id: room_id,
            },
          )
          room_id += 1
        }
      }
    }
  }
  rooms
}
