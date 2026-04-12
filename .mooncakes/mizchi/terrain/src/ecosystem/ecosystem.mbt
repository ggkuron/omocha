///|
pub(all) struct EcosystemConfig {
  width : Int
  height : Int
  num_zones : Int
  zone_size : Int
}

///|
pub fn EcosystemConfig::default() -> EcosystemConfig {
  { width: 50, height: 30, num_zones: 3, zone_size: 8 }
}

///|
pub(all) struct EcosystemResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : EcosystemConfig,
  rng : @types.RNG
) -> EcosystemResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let rooms : Array[@types.Room] = []
  let zone_centers : Array[@types.Point] = []
  // Biome types: 0=cavern(Floor), 1=river(Water), 2=lava(Key)
  let zone_types : Array[Int] = []
  // Place zones with guaranteed biome variety
  let margin = config.zone_size + 2
  for i in 0..<config.num_zones {
    let cx = rng.rand_range(margin, w - margin)
    let cy = rng.rand_range(margin, h - margin)
    zone_centers.push({ x: cx, y: cy })
    // Cycle through biome types to ensure variety
    zone_types.push(i % 3)
  }
  // Carve each zone as a blob using random walk
  for i in 0..<config.num_zones {
    let center = zone_centers[i]
    let biome = zone_types[i]
    let tile : @types.Tile = match biome {
      0 => @types.Floor
      1 => @types.Water
      _ => @types.Key
    }
    // Carve a blob around the center
    let size = config.zone_size
    let area = size * size
    let mut carved = 0
    let mut x = center.x
    let mut y = center.y
    while carved < area {
      if grid.in_bounds(x, y) {
        grid.set(x, y, tile)
        carved += 1
      }
      // Random walk
      let dir = rng.rand_direction()
      let nx = x + dir.x
      let ny = y + dir.y
      if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 {
        x = nx
        y = ny
      }
    }
    // Record zone as a room
    let half = size / 2
    let rx = if center.x - half > 0 { center.x - half } else { 0 }
    let ry = if center.y - half > 0 { center.y - half } else { 0 }
    rooms.push(
      {
        rect: { x: rx, y: ry, w: size, h: size },
        id: i,
      },
    )
  }
  // Connect zones with corridors
  for i in 0..<(config.num_zones - 1) {
    let from = zone_centers[i]
    let to = zone_centers[i + 1]
    carve_corridor(grid, from, to, w, h)
  }
  // Also connect last to first for a loop
  if config.num_zones > 2 {
    let from = zone_centers[config.num_zones - 1]
    let to = zone_centers[0]
    carve_corridor(grid, from, to, w, h)
  }
  // Add transition borders between biome zones
  // Scan and add Wall borders where different biomes meet
  let snapshot = grid.clone()
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      let current = snapshot.get(x, y)
      if current == @types.Wall || current == @types.Corridor {
        continue
      }
      // Check if any neighbor has a different non-wall tile
      let neighbors = snapshot.neighbors4(x, y)
      for n in neighbors {
        let nt = snapshot.get(n.x, n.y)
        if nt != @types.Wall && nt != @types.Corridor && nt != current {
          // Transition border - add a door
          if rng.rand_bool(0.3) {
            grid.set(x, y, @types.Door)
          }
          break
        }
      }
    }
  }
  // Place Start in first cavern zone, Goal in last
  let start_zone = zone_centers[0]
  let start_pos = find_nearest(grid, start_zone.x, start_zone.y, @types.Floor)
  grid.set(start_pos.x, start_pos.y, @types.Start)
  let goal_zone = zone_centers[config.num_zones - 1]
  let goal_tile : @types.Tile = match zone_types[config.num_zones - 1] {
    0 => @types.Floor
    1 => @types.Water
    _ => @types.Key
  }
  let goal_pos = find_nearest(grid, goal_zone.x, goal_zone.y, goal_tile)
  if goal_pos.x != start_pos.x || goal_pos.y != start_pos.y {
    grid.set(goal_pos.x, goal_pos.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_corridor(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  _w : Int,
  _h : Int
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if grid.in_bounds(x, y) && grid.get(x, y) == @types.Wall {
      grid.set(x, y, @types.Corridor)
    }
    if x != to.x {
      if x < to.x {
        x += 1
      } else {
        x -= 1
      }
    } else if y != to.y {
      if y < to.y {
        y += 1
      } else {
        y -= 1
      }
    }
  }
  if grid.in_bounds(to.x, to.y) && grid.get(to.x, to.y) == @types.Wall {
    grid.set(to.x, to.y, @types.Corridor)
  }
}

///|
fn find_nearest(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int,
  tile : @types.Tile
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
