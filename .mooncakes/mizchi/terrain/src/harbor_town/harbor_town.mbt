///|
pub(all) struct HarborTownConfig {
  width : Int
  height : Int
  max_height : Int
  water_ratio : Double
  building_density : Double
}

///|
pub fn HarborTownConfig::default() -> HarborTownConfig {
  {
    width: 48,
    height: 30,
    max_height: 6,
    water_ratio: 0.3,
    building_density: 0.6,
  }
}

///|
pub(all) struct HarborTownResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : HarborTownConfig,
  rng : @types.RNG
) -> HarborTownResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Floor, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Calculate the water boundary (left portion is sea)
  let water_end = (w.to_double() * config.water_ratio).to_int().max(3)
  // Generate a jagged coastline
  let coastline = Array::make(h, water_end)
  for y in 0..<h {
    let jag = rng.rand_range(-2, 3)
    coastline[y] = (water_end + jag).max(2).min(w - 6)
  }
  // Smooth the coastline slightly
  let smoothed = Array::make(h, water_end)
  for y in 0..<h {
    let prev = if y > 0 { coastline[y - 1] } else { coastline[y] }
    let next = if y < h - 1 { coastline[y + 1] } else { coastline[y] }
    smoothed[y] = (prev + coastline[y] + next) / 3
  }
  // Fill water (sea) on the left side
  for y in 0..<h {
    let coast_x = smoothed[y]
    for x in 0..<coast_x {
      if grid.in_bounds(x, y) {
        grid.set(x, y, @types.Water)
        grid.set_height(x, y, 0)
      }
    }
  }
  // Generate terrain height: rising slope from coastline to the right
  for y in 0..<h {
    let coast_x = smoothed[y]
    let land_width = w - coast_x
    for x in coast_x..<w {
      if grid.in_bounds(x, y) {
        let dist_from_coast = x - coast_x
        let ratio = dist_from_coast.to_double() / land_width.to_double()
        let base_h = (ratio * config.max_height.to_double()).to_int()
        // Add slight noise
        let noise = rng.rand_range(-1, 2)
        let final_h = (base_h + noise).max(0).min(config.max_height)
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, final_h)
      }
    }
  }
  // Build docks/piers extending into the water
  let num_piers = rng.rand_range(2, 4)
  let pier_spacing = h / (num_piers + 1)
  for i in 0..<num_piers {
    let pier_y = pier_spacing * (i + 1) + rng.rand_range(-1, 2)
    let clamped_pier_y = pier_y.max(1).min(h - 2)
    let coast_x = smoothed[clamped_pier_y]
    let pier_length = rng.rand_range(3, 6)
    let pier_start_x = (coast_x - pier_length).max(0)
    // Pier is a corridor at height 1 (just above water)
    for x in pier_start_x..<coast_x {
      if grid.in_bounds(x, clamped_pier_y) {
        grid.set(x, clamped_pier_y, @types.Corridor)
        grid.set_height(x, clamped_pier_y, 1)
      }
    }
    // Also make the pier 1 cell wide, add walls on sides for railing
    let rail_y1 = clamped_pier_y - 1
    let rail_y2 = clamped_pier_y + 1
    if grid.in_bounds(pier_start_x, rail_y1) {
      // Just mark the pier endpoint
      grid.set(pier_start_x, clamped_pier_y, @types.Corridor)
      grid.set_height(pier_start_x, clamped_pier_y, 1)
    }
    ignore(rail_y1)
    ignore(rail_y2)
  }
  // Create contour streets running roughly horizontal (along height levels)
  // Streets are placed at each height level boundary
  for level in 1..=config.max_height {
    for y in 0..<h {
      let coast_x = smoothed[y]
      let land_width = w - coast_x
      // Find x position where height transitions to this level
      let transition_x = coast_x +
        (level.to_double() / config.max_height.to_double() *
        land_width.to_double()).to_int()
      // Place a horizontal street corridor near this transition
      let street_x_start = (transition_x - 1).max(coast_x)
      let street_x_end = (transition_x + 1).min(w - 1)
      for x in street_x_start..=street_x_end {
        if grid.in_bounds(x, y) {
          let tile = grid.get(x, y)
          if tile == @types.Floor {
            grid.set(x, y, @types.Corridor)
            // Street height matches the terrain
          }
        }
      }
    }
  }
  // Add some vertical connecting streets (perpendicular to contour)
  let num_vertical_streets = rng.rand_range(3, 6)
  let v_street_spacing = h / (num_vertical_streets + 1)
  for i in 0..<num_vertical_streets {
    let street_y = v_street_spacing * (i + 1) + rng.rand_range(-1, 2)
    let clamped_y = street_y.max(0).min(h - 1)
    let coast_x = smoothed[clamped_y]
    for x in coast_x..<w {
      if grid.in_bounds(x, clamped_y) {
        let tile = grid.get(x, clamped_y)
        if tile == @types.Floor || tile == @types.Corridor {
          grid.set(x, clamped_y, @types.Corridor)
        }
      }
    }
  }
  // Place buildings on the slope
  // Buildings are placed between streets, respecting building density
  let min_building = 3
  let max_building = 5
  let mut attempts = 0
  let max_attempts = w * h / 4
  while attempts < max_attempts {
    attempts += 1
    if not(rng.rand_bool(config.building_density)) {
      continue
    }
    let bw = rng.rand_range(min_building, max_building + 1)
    let bh = rng.rand_range(min_building, max_building + 1)
    let bx = rng.rand_range(0, w - bw)
    let by = rng.rand_range(0, h - bh)
    // Check that the building is on land (not water), not on streets
    let mut valid = true
    let mut total_height = 0
    let mut count = 0
    for dy in 0..<bh {
      for dx in 0..<bw {
        let px = bx + dx
        let py = by + dy
        if grid.in_bounds(px, py) {
          let tile = grid.get(px, py)
          if tile == @types.Water || tile == @types.Corridor ||
            tile == @types.Wall || tile == @types.Door ||
            tile == @types.Start || tile == @types.Goal {
            valid = false
          }
          total_height += grid.get_height(px, py)
          count += 1
        } else {
          valid = false
        }
      }
    }
    if not(valid) || count == 0 {
      continue
    }
    // Check no overlap with existing buildings
    let building_rect : @types.Rect = { x: bx, y: by, w: bw, h: bh }
    let mut overlaps = false
    for existing in rooms {
      let expanded : @types.Rect = {
        x: existing.rect.x - 1,
        y: existing.rect.y - 1,
        w: existing.rect.w + 2,
        h: existing.rect.h + 2,
      }
      if building_rect.intersects(expanded) {
        overlaps = true
        break
      }
    }
    if overlaps {
      continue
    }
    // Average height for the building
    let avg_h = total_height / count
    // Building must be on land (height > 0)
    if avg_h == 0 {
      continue
    }
    // Carve the building: walls on border, floor inside
    for dy in 0..<bh {
      for dx in 0..<bw {
        let px = bx + dx
        let py = by + dy
        if grid.in_bounds(px, py) {
          if dx == 0 || dx == bw - 1 || dy == 0 || dy == bh - 1 {
            grid.set(px, py, @types.Wall)
            grid.set_height(px, py, avg_h + 1)
          } else {
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, avg_h)
          }
        }
      }
    }
    // Place a door on the side facing downhill (toward the coast)
    let door_dy = rng.rand_range(1, bh - 1)
    let door_x = bx
    let door_y = by + door_dy
    if grid.in_bounds(door_x, door_y) {
      grid.set(door_x, door_y, @types.Door)
      grid.set_height(door_x, door_y, avg_h)
    }
    rooms.push({ rect: building_rect, id: room_id })
    room_id += 1
  }
  // Place Start at the dock area
  let start_y = h / 2
  let start_coast_x = smoothed[start_y]
  let start_x = start_coast_x
  if grid.in_bounds(start_x, start_y) {
    grid.set(start_x, start_y, @types.Start)
    grid.set_height(start_x, start_y, 1)
  }
  // Place Goal at the top of the hill (rightmost, highest point area)
  let goal_x = w - 2
  let goal_y = h / 2
  if grid.in_bounds(goal_x, goal_y) {
    grid.set(goal_x, goal_y, @types.Goal)
  }
  ignore(room_id)
  { grid, rooms }
}
