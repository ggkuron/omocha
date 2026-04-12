///|
pub(all) struct CanyonConfig {
  width : Int
  height : Int
  max_height : Int
  canyon_width : Int
  num_bridges : Int
  cave_chance : Double
}

///|
pub fn CanyonConfig::default() -> CanyonConfig {
  {
    width: 48,
    height: 30,
    max_height: 8,
    canyon_width: 6,
    num_bridges: 3,
    cave_chance: 0.3,
  }
}

///|
pub(all) struct CanyonResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : CanyonConfig, rng : @types.RNG) -> CanyonResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Fill the entire grid with high terrain (walls at max height)
  for y in 0..<h {
    for x in 0..<w {
      grid.set(x, y, @types.Wall)
      grid.set_height(x, y, config.max_height)
    }
  }
  // Carve the canyon through the center of the map, running top to bottom
  // The canyon meanders slightly using a random walk
  let half_canyon = config.canyon_width / 2
  let canyon_center_x = Array::make(h, w / 2)
  // Generate meandering canyon path
  let mut cx = w / 2
  for y in 0..<h {
    // Random walk for canyon center
    let drift = rng.rand_range(-1, 2)
    cx = (cx + drift).max(half_canyon + 2).min(w - half_canyon - 3)
    canyon_center_x[y] = cx
  }
  // Carve the canyon floor and walls
  for y in 0..<h {
    let center = canyon_center_x[y]
    for x in (center - half_canyon)..<(center + half_canyon) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, 0)
      }
    }
    // Canyon wall slopes: gradually increase height from canyon edge outward
    let slope_width = 3
    for s in 0..<slope_width {
      let left_x = center - half_canyon - 1 - s
      let right_x = center + half_canyon + s
      let slope_h = ((s + 1) * config.max_height / (slope_width + 1)).min(
        config.max_height,
      )
      if grid.in_bounds(left_x, y) {
        grid.set(left_x, y, @types.Wall)
        grid.set_height(left_x, y, slope_h)
      }
      if grid.in_bounds(right_x, y) {
        grid.set(right_x, y, @types.Wall)
        grid.set_height(right_x, y, slope_h)
      }
    }
  }
  // Add water at the bottom of the canyon (a river)
  for y in 0..<h {
    let center = canyon_center_x[y]
    // Water runs through the middle of the canyon floor
    let water_half = (half_canyon / 3).max(1)
    for x in (center - water_half)..<(center + water_half) {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Water)
        grid.set_height(x, y, 0)
      }
    }
  }
  // Carve caves into canyon walls at various heights
  let cave_positions : Array[(Int, Int, Int)] = [] // (x, y, side) side: 0=left, 1=right
  for y in 2..<(h - 2) {
    let center = canyon_center_x[y]
    // Try left wall cave
    if rng.rand_bool(config.cave_chance) {
      let cave_x = center - half_canyon - 1
      let cave_y = y
      if grid.in_bounds(cave_x, cave_y) {
        cave_positions.push((cave_x, cave_y, 0))
      }
    }
    // Try right wall cave
    if rng.rand_bool(config.cave_chance) {
      let cave_x = center + half_canyon
      let cave_y = y
      if grid.in_bounds(cave_x, cave_y) {
        cave_positions.push((cave_x, cave_y, 1))
      }
    }
  }
  // Actually carve caves (small rooms dug into the walls)
  for cave in cave_positions {
    let cave_x = cave.0
    let cave_y = cave.1
    let side = cave.2
    let cave_w = rng.rand_range(3, 6)
    let cave_h = rng.rand_range(2, 4)
    let cave_height = rng.rand_range(1, config.max_height / 2 + 1)
    let start_x = if side == 0 { cave_x - cave_w } else { cave_x + 1 }
    let start_y = cave_y - cave_h / 2
    // Check bounds
    if start_x < 0 || start_x + cave_w >= w || start_y < 0 ||
      start_y + cave_h >= h {
      continue
    }
    // Check no overlap with existing rooms
    let cave_rect : @types.Rect = {
      x: start_x,
      y: start_y,
      w: cave_w,
      h: cave_h,
    }
    let mut overlaps = false
    for existing in rooms {
      let expanded : @types.Rect = {
        x: existing.rect.x - 1,
        y: existing.rect.y - 1,
        w: existing.rect.w + 2,
        h: existing.rect.h + 2,
      }
      if cave_rect.intersects(expanded) {
        overlaps = true
        break
      }
    }
    if not(overlaps) {
      // Carve the cave
      for dy in 0..<cave_h {
        for dx in 0..<cave_w {
          let px = start_x + dx
          let py = start_y + dy
          if grid.in_bounds(px, py) {
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, cave_height)
          }
        }
      }
      // Place a door at the cave entrance (where it meets the canyon wall)
      if grid.in_bounds(cave_x, cave_y) {
        grid.set(cave_x, cave_y, @types.Door)
        grid.set_height(cave_x, cave_y, cave_height)
      }
      rooms.push({ rect: cave_rect, id: room_id })
      room_id += 1
    }
  }
  // Build bridges across the canyon
  let bridge_spacing = h / (config.num_bridges + 1)
  for i in 0..<config.num_bridges {
    let bridge_y = bridge_spacing * (i + 1)
    if bridge_y >= h {
      continue
    }
    let center = canyon_center_x[bridge_y]
    let bridge_h = rng.rand_range(config.max_height / 2, config.max_height)
    // Bridge spans from left wall to right wall
    let bridge_start_x = center - half_canyon - 1
    let bridge_end_x = center + half_canyon
    for x in bridge_start_x..=bridge_end_x {
      if grid.in_bounds(x, bridge_y) {
        let cur_tile = grid.get(x, bridge_y)
        // Only bridge over canyon floor/water, don't overwrite walls
        if cur_tile == @types.Floor || cur_tile == @types.Water {
          grid.set(x, bridge_y, @types.Corridor)
          grid.set_height(x, bridge_y, bridge_h)
        }
      }
    }
    // Add bridge supports (Wall pillars at the edges)
    if grid.in_bounds(bridge_start_x, bridge_y) {
      grid.set(bridge_start_x, bridge_y, @types.Wall)
      grid.set_height(bridge_start_x, bridge_y, bridge_h)
    }
    if grid.in_bounds(bridge_end_x, bridge_y) {
      grid.set(bridge_end_x, bridge_y, @types.Wall)
      grid.set_height(bridge_end_x, bridge_y, bridge_h)
    }
  }
  // Place walkable ledges along canyon walls
  // Narrow corridors that run along the wall at mid-height
  let ledge_height = config.max_height / 2
  for y in 0..<h {
    let center = canyon_center_x[y]
    // Left ledge
    let left_ledge_x = center - half_canyon
    if grid.in_bounds(left_ledge_x, y) &&
      grid.get(left_ledge_x, y) == @types.Floor {
      grid.set(left_ledge_x, y, @types.Corridor)
      grid.set_height(left_ledge_x, y, ledge_height)
    }
    // Right ledge
    let right_ledge_x = center + half_canyon - 1
    if grid.in_bounds(right_ledge_x, y) &&
      grid.get(right_ledge_x, y) == @types.Floor {
      grid.set(right_ledge_x, y, @types.Corridor)
      grid.set_height(right_ledge_x, y, ledge_height)
    }
  }
  // Place Start at the top of the canyon, Goal at the bottom
  let start_center = canyon_center_x[1]
  if grid.in_bounds(start_center, 1) {
    grid.set(start_center, 1, @types.Start)
    // Start on the ledge
    grid.set_height(start_center, 1, ledge_height)
  }
  let goal_center = canyon_center_x[h - 2]
  if grid.in_bounds(goal_center, h - 2) {
    grid.set(goal_center, h - 2, @types.Goal)
    grid.set_height(goal_center, h - 2, ledge_height)
  }
  ignore(room_id)
  { grid, rooms }
}
