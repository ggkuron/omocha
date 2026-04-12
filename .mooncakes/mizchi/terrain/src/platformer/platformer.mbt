///|
pub(all) struct PlatformerConfig {
  width : Int
  height : Int
  num_platforms : Int
  platform_min_len : Int
  platform_max_len : Int
  ladder_chance : Double
  gap_chance : Double
  water_level : Int
}

///|
pub fn PlatformerConfig::default() -> PlatformerConfig {
  {
    width: 60,
    height: 20,
    num_platforms: 15,
    platform_min_len: 3,
    platform_max_len: 8,
    ladder_chance: 0.3,
    gap_chance: 0.2,
    water_level: 2,
  }
}

///|
pub(all) struct PlatformerResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
priv struct Platform {
  x : Int
  y : Int
  len : Int
}

///|
pub fn generate(config : PlatformerConfig, rng : @types.RNG) -> PlatformerResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Empty)
  let rooms : Array[@types.Room] = []
  // Fill bottom with water
  for y in (config.height - config.water_level)..<config.height {
    for x in 0..<config.width {
      grid.set(x, y, @types.Water)
    }
  }
  // Place ground floor
  let ground_y = config.height - config.water_level - 1
  for x in 0..<config.width {
    grid.set(x, ground_y, @types.Floor)
  }
  // Generate platforms at varying heights
  let platforms : Array[Platform] = []
  // Add ground as first platform
  platforms.push({ x: 0, y: ground_y, len: config.width })
  rooms.push(
    {
      rect: { x: 0, y: ground_y, w: config.width, h: 1 },
      id: 0,
    },
  )
  // Place additional platforms
  let usable_height = ground_y - 2 // leave top margin
  for i in 0..<config.num_platforms {
    let plat_len = rng.rand_range(config.platform_min_len, config.platform_max_len + 1)
    let plat_x = rng.rand_range(1, config.width - plat_len - 1)
    // Distribute platforms vertically
    let min_y = 2
    let plat_y = rng.rand_range(min_y, usable_height)
    // Check for gap chance - skip placing this platform sometimes
    if rng.rand_bool(config.gap_chance) {
      continue
    }
    // Place the platform
    for x in plat_x..<(plat_x + plat_len) {
      grid.set(x, plat_y, @types.Floor)
    }
    // Place wall underneath for visual support (1 tile)
    if plat_y + 1 < ground_y {
      // Add a wall tile at each end as support pillars
      if grid.in_bounds(plat_x, plat_y + 1) {
        grid.set(plat_x, plat_y + 1, @types.Wall)
      }
      if grid.in_bounds(plat_x + plat_len - 1, plat_y + 1) {
        grid.set(plat_x + plat_len - 1, plat_y + 1, @types.Wall)
      }
    }
    platforms.push({ x: plat_x, y: plat_y, len: plat_len })
    rooms.push(
      {
        rect: { x: plat_x, y: plat_y, w: plat_len, h: 1 },
        id: i + 1,
      },
    )
  }
  // Place ladders connecting platforms
  // Sort platforms by y coordinate (top to bottom)
  let sorted_platforms = platforms.copy()
  for i in 0..<sorted_platforms.length() {
    for j = i + 1; j < sorted_platforms.length(); {
      if sorted_platforms[j].y < sorted_platforms[i].y {
        sorted_platforms.swap(i, j)
      }
      continue j + 1
    }
  }
  // Connect adjacent platforms with ladders
  for i in 0..<(sorted_platforms.length() - 1) {
    if not(rng.rand_bool(config.ladder_chance)) {
      continue
    }
    let upper = sorted_platforms[i]
    let lower = sorted_platforms[i + 1]
    // Find overlap region
    let overlap_start = if upper.x > lower.x { upper.x } else { lower.x }
    let upper_end = upper.x + upper.len
    let lower_end = lower.x + lower.len
    let overlap_end = if upper_end < lower_end { upper_end } else { lower_end }
    if overlap_start < overlap_end {
      // Place ladder in overlap area
      let ladder_x = rng.rand_range(overlap_start, overlap_end)
      for y in (upper.y + 1)..<lower.y {
        if grid.in_bounds(ladder_x, y) {
          grid.set(ladder_x, y, @types.Corridor)
        }
      }
    } else {
      // No overlap - place a ladder at the edge of the upper platform
      let ladder_x = upper.x + upper.len / 2
      for y in (upper.y + 1)..<lower.y {
        if grid.in_bounds(ladder_x, y) {
          grid.set(ladder_x, y, @types.Corridor)
        }
      }
    }
  }
  // Ensure traversability: add ladders from ground to at least one platform
  // Find lowest non-ground platform
  let mut found_connection = false
  for i in 0..<sorted_platforms.length() {
    if sorted_platforms[i].y < ground_y && not(found_connection) {
      let plat = sorted_platforms[i]
      let ladder_x = plat.x + plat.len / 2
      for y in (plat.y + 1)..<ground_y {
        if grid.in_bounds(ladder_x, y) && grid.get(ladder_x, y) == @types.Empty {
          grid.set(ladder_x, y, @types.Corridor)
        }
      }
      found_connection = true
    }
  }
  // Place Start at left side of ground, Goal at right side
  grid.set(1, ground_y - 1, @types.Start)
  grid.set(config.width - 2, ground_y - 1, @types.Goal)
  { grid, rooms }
}
