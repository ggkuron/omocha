///|
pub(all) struct IsoCityConfig {
  width : Int
  height : Int
  max_height : Int
  building_density : Double
  street_width : Int
}

///|
pub fn IsoCityConfig::default() -> IsoCityConfig {
  { width: 48, height: 48, max_height: 8, building_density: 0.7, street_width: 2 }
}

///|
pub(all) struct IsoCityResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : IsoCityConfig, rng : @types.RNG) -> IsoCityResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  let sw = config.street_width
  // Determine block size: street grid spacing
  let block_size = sw + 6 // block interior + street
  // Lay down streets in a grid pattern
  // Horizontal streets
  let mut sy = 0
  while sy < h {
    for row in 0..<sw {
      let y = sy + row
      if y < h {
        for x in 0..<w {
          grid.set(x, y, @types.Corridor)
          grid.set_height(x, y, 0)
        }
      }
    }
    sy = sy + block_size
  }
  // Vertical streets
  let mut sx = 0
  while sx < w {
    for col in 0..<sw {
      let x = sx + col
      if x < w {
        for y in 0..<h {
          grid.set(x, y, @types.Corridor)
          grid.set_height(x, y, 0)
        }
      }
    }
    sx = sx + block_size
  }
  // Fill blocks with buildings
  let mut room_id = 0
  let mut by = sw
  while by < h {
    let block_h = if by + block_size - sw <= h {
      block_size - sw
    } else {
      h - by
    }
    let mut bx = sw
    while bx < w {
      let block_w = if bx + block_size - sw <= w {
        block_size - sw
      } else {
        w - bx
      }
      if block_w >= 3 && block_h >= 3 {
        // Decide whether to place a building in this block
        if rng.rand_bool(config.building_density) {
          // Building occupies inner part of block with 1-cell margin
          let margin = 1
          let bld_x = bx + margin
          let bld_y = by + margin
          let bld_w = block_w - margin * 2
          let bld_h = block_h - margin * 2
          if bld_w >= 2 && bld_h >= 2 {
            let building_height = rng.rand_range(1, config.max_height + 1)
            // Fill building footprint with Floor at building height
            for dy in 0..<bld_h {
              for dx in 0..<bld_w {
                let px = bld_x + dx
                let py = bld_y + dy
                if px < w && py < h {
                  grid.set(px, py, @types.Floor)
                  grid.set_height(px, py, building_height)
                }
              }
            }
            // Add walls around the building base
            for dx in 0..<bld_w {
              let px = bld_x + dx
              let top_y = bld_y - 1
              let bot_y = bld_y + bld_h
              if px < w && top_y >= 0 {
                if grid.get(px, top_y) != @types.Corridor {
                  grid.set(px, top_y, @types.Wall)
                  grid.set_height(px, top_y, building_height)
                }
              }
              if px < w && bot_y < h {
                if grid.get(px, bot_y) != @types.Corridor {
                  grid.set(px, bot_y, @types.Wall)
                  grid.set_height(px, bot_y, building_height)
                }
              }
            }
            for dy in 0..<bld_h {
              let py = bld_y + dy
              let left_x = bld_x - 1
              let right_x = bld_x + bld_w
              if py < h && left_x >= 0 {
                if grid.get(left_x, py) != @types.Corridor {
                  grid.set(left_x, py, @types.Wall)
                  grid.set_height(left_x, py, building_height)
                }
              }
              if py < h && right_x < w {
                if grid.get(right_x, py) != @types.Corridor {
                  grid.set(right_x, py, @types.Wall)
                  grid.set_height(right_x, py, building_height)
                }
              }
            }
            // Place a door on one side of the building facing a street
            let door_side = rng.next_int(4)
            let mut door_x = bld_x
            let mut door_y = bld_y
            match door_side {
              0 => {
                // top
                door_x = bld_x + rng.next_int(bld_w)
                door_y = bld_y - 1
              }
              1 => {
                // bottom
                door_x = bld_x + rng.next_int(bld_w)
                door_y = bld_y + bld_h
              }
              2 => {
                // left
                door_x = bld_x - 1
                door_y = bld_y + rng.next_int(bld_h)
              }
              _ => {
                // right
                door_x = bld_x + bld_w
                door_y = bld_y + rng.next_int(bld_h)
              }
            }
            if door_x >= 0 && door_x < w && door_y >= 0 && door_y < h {
              grid.set(door_x, door_y, @types.Door)
              grid.set_height(door_x, door_y, 0)
            }
            rooms.push(
              {
                rect: { x: bld_x, y: bld_y, w: bld_w, h: bld_h },
                id: room_id,
              },
            )
            room_id += 1
          }
        } else {
          // Empty lot - ground level open space
          for dy in 0..<block_h {
            for dx in 0..<block_w {
              let px = bx + dx
              let py = by + dy
              if px < w && py < h {
                grid.set(px, py, @types.Floor)
                grid.set_height(px, py, 0)
              }
            }
          }
        }
      }
      bx = bx + block_size
    }
    by = by + block_size
  }
  // Place Start and Goal on streets
  place_start_goal_on_corridors(grid, w, h)
  { grid, rooms }
}

///|
fn place_start_goal_on_corridors(
  grid : @types.HeightGrid,
  w : Int,
  h : Int
) -> Unit {
  let mut start_placed = false
  // Start: top-left corner area
  for y in 0..<h {
    for x in 0..<w {
      if not(start_placed) && grid.get(x, y) == @types.Corridor {
        grid.set(x, y, @types.Start)
        start_placed = true
      }
    }
  }
  // Goal: bottom-right corner area
  let mut goal_placed = false
  for y = h - 1; y >= 0; y = y - 1 {
    for x = w - 1; x >= 0; x = x - 1 {
      if not(goal_placed) && grid.get(x, y) == @types.Corridor {
        grid.set(x, y, @types.Goal)
        goal_placed = true
      }
    }
  }
}
