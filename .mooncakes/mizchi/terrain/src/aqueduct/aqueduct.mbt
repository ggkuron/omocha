///|
pub(all) struct AqueductConfig {
  width : Int
  height : Int
  max_height : Int
  num_arches : Int
  terrain_roughness : Double
}

///|
pub fn AqueductConfig::default() -> AqueductConfig {
  { width: 60, height: 30, max_height: 8, num_arches: 6, terrain_roughness: 0.3 }
}

///|
pub(all) struct AqueductResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : AqueductConfig, rng : @types.RNG) -> AqueductResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  // Generate terrain heights using midpoint-style noise
  // Terrain slopes from left (high) to right (low)
  let terrain = Array::make(w, 0)
  let base_left = config.max_height - 2
  let base_right = 1
  for x in 0..<w {
    let t = x.to_double() / (w - 1).to_double()
    let base = base_left.to_double() * (1.0 - t) + base_right.to_double() * t
    let noise = if rng.rand_bool(config.terrain_roughness) {
      rng.rand_range(-1, 2)
    } else {
      0
    }
    terrain[x] = (base + noise.to_double()).to_int().max(1).min(config.max_height - 1)
  }
  // Smooth terrain
  for _pass in 0..<2 {
    for x in 1..<(w - 1) {
      terrain[x] = (terrain[x - 1] + terrain[x] + terrain[x + 1]) / 3
    }
  }
  // Draw ground terrain
  for x in 0..<w {
    let ground_h = terrain[x]
    // Fill from bottom up to ground height
    for y in 0..<h {
      let y_from_bottom = h - 1 - y
      if y_from_bottom < ground_h {
        grid.set(x, y, @types.Wall)
        grid.set_height(x, y, y_from_bottom + 1)
      } else if y_from_bottom == ground_h {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, ground_h)
      }
    }
  }
  // Aqueduct water channel runs horizontally at a fixed height near the top
  let channel_h = config.max_height - 1
  let channel_y = 2 // Near top of map in grid coords
  // Calculate arch positions evenly spaced
  let arch_spacing = w / (config.num_arches + 1)
  let arch_positions : Array[Int] = []
  for i in 1..=(config.num_arches) {
    let arch_x = i * arch_spacing
    if arch_x > 1 && arch_x < w - 2 {
      arch_positions.push(arch_x)
    }
  }
  // Build arch supports from ground to channel
  let mut room_id = 0
  for arch_x in arch_positions {
    let ground_level = terrain[arch_x]
    let ground_y = h - 1 - ground_level // Grid y of ground surface
    // Build arch column (Wall from ground to channel)
    let arch_width = 3
    let half_w = arch_width / 2
    for dx in -half_w..=half_w {
      let px = arch_x + dx
      if px >= 0 && px < w {
        for y = ground_y; y >= channel_y; y = y - 1 {
          let col_h = h - 1 - y
          grid.set(px, y, @types.Wall)
          grid.set_height(px, y, col_h.min(config.max_height))
        }
      }
    }
    // Create arch opening (door-like gap near ground)
    let arch_opening_h = ((h - 1 - ground_y - channel_y) / 3).max(2)
    let opening_start_y = ground_y - 1
    for dy in 0..<arch_opening_h {
      let oy = opening_start_y - dy
      if oy > channel_y && oy < h && arch_x >= 0 && arch_x < w {
        let open_h = h - 1 - oy
        grid.set(arch_x, oy, @types.Corridor)
        grid.set_height(arch_x, oy, open_h.min(config.max_height))
      }
    }
    // Side structures at arch bases
    let struct_w = 3
    let struct_h_dim = 2
    // Left side structure
    let left_sx = arch_x - half_w - struct_w
    if left_sx >= 0 {
      let sy = ground_y - struct_h_dim + 1
      if sy >= 0 && sy < h {
        for sy2 in sy..=ground_y {
          for sx2 in left_sx..<(left_sx + struct_w) {
            if sx2 >= 0 && sx2 < w && sy2 >= 0 && sy2 < h {
              let tile = if sy2 == sy || sy2 == ground_y || sx2 == left_sx ||
                sx2 == left_sx + struct_w - 1 {
                @types.Wall
              } else {
                @types.Floor
              }
              let sh = ground_level + (ground_y - sy2)
              grid.set(sx2, sy2, tile)
              grid.set_height(sx2, sy2, sh.min(config.max_height))
            }
          }
        }
        rooms.push(
          {
            rect: { x: left_sx, y: sy, w: struct_w, h: struct_h_dim },
            id: room_id,
          },
        )
        room_id += 1
      }
    }
    // Right side structure
    let right_sx = arch_x + half_w + 1
    if right_sx + struct_w <= w {
      let sy = ground_y - struct_h_dim + 1
      if sy >= 0 && sy < h {
        for sy2 in sy..=ground_y {
          for sx2 in right_sx..<(right_sx + struct_w) {
            if sx2 >= 0 && sx2 < w && sy2 >= 0 && sy2 < h {
              let tile = if sy2 == sy || sy2 == ground_y || sx2 == right_sx ||
                sx2 == right_sx + struct_w - 1 {
                @types.Wall
              } else {
                @types.Floor
              }
              let sh = ground_level + (ground_y - sy2)
              grid.set(sx2, sy2, tile)
              grid.set_height(sx2, sy2, sh.min(config.max_height))
            }
          }
        }
        rooms.push(
          {
            rect: { x: right_sx, y: sy, w: struct_w, h: struct_h_dim },
            id: room_id,
          },
        )
        room_id += 1
      }
    }
  }
  // Draw water channel along the top
  for x in 0..<w {
    if channel_y >= 0 && channel_y < h {
      // Check if an arch support exists here
      let cur = grid.get(x, channel_y)
      if cur != @types.Wall {
        grid.set(x, channel_y, @types.Water)
        grid.set_height(x, channel_y, channel_h)
      }
    }
    // Channel walls (one row above and below)
    if channel_y - 1 >= 0 {
      let cur_above = grid.get(x, channel_y - 1)
      if cur_above == @types.Empty {
        grid.set(x, channel_y - 1, @types.Wall)
        grid.set_height(x, channel_y - 1, channel_h + 1)
      }
    }
    if channel_y + 1 < h {
      let cur_below = grid.get(x, channel_y + 1)
      if cur_below == @types.Empty {
        grid.set(x, channel_y + 1, @types.Wall)
        grid.set_height(x, channel_y + 1, channel_h)
      }
    }
  }
  // Place Start and Goal
  // Start near left ground level
  let start_y = h - 1 - terrain[1]
  if start_y >= 0 && start_y < h {
    grid.set(1, start_y, @types.Start)
    grid.set_height(1, start_y, terrain[1])
  }
  // Goal near right ground level
  let goal_x = w - 2
  let goal_y = h - 1 - terrain[goal_x]
  if goal_y >= 0 && goal_y < h {
    grid.set(goal_x, goal_y, @types.Goal)
    grid.set_height(goal_x, goal_y, terrain[goal_x])
  }
  { grid, rooms }
}
