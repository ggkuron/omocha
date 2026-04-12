///|
pub(all) struct UnderwaterBaseConfig {
  width : Int
  height : Int
  depth : Int
  room_density : Double
}

///|
pub fn UnderwaterBaseConfig::default() -> UnderwaterBaseConfig {
  { width: 40, height: 20, depth: 4, room_density: 0.4 }
}

///|
pub(all) struct UnderwaterBaseResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
fn try_place_room(
  grid : @types.Grid3D,
  z : Int,
  rect : @types.Rect,
  rooms : Array[@types.Room],
  room_id : Int
) -> Bool {
  // Check bounds
  if rect.x < 1 ||
    rect.y < 1 ||
    rect.x + rect.w >= grid.width - 1 ||
    rect.y + rect.h >= grid.height - 1 {
    return false
  }
  // Check for overlap with existing rooms on this layer
  let layer = grid.get_layer(z)
  for y in rect.y..<(rect.y + rect.h) {
    for x in rect.x..<(rect.x + rect.w) {
      if layer.get(x, y) != @types.Water {
        return false
      }
    }
  }
  // Carve the room
  for y in rect.y..<(rect.y + rect.h) {
    for x in rect.x..<(rect.x + rect.w) {
      // Walls on perimeter, floor inside
      if x == rect.x ||
        x == rect.x + rect.w - 1 ||
        y == rect.y ||
        y == rect.y + rect.h - 1 {
        grid.set(x, y, z, @types.Wall)
      } else {
        grid.set(x, y, z, @types.Floor)
      }
    }
  }
  rooms.push({ rect, id: room_id })
  true
}

///|
pub fn generate(
  config : UnderwaterBaseConfig,
  rng : @types.RNG
) -> UnderwaterBaseResult {
  let width = config.width
  let height = config.height
  let depth_count = config.depth
  let grid = @types.Grid3D::make(width, height, depth_count, @types.Water)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Top layer (z=0) has docking bay - larger room at center
  let dock_w = width / 3
  let dock_h = height / 3
  let dock_rect : @types.Rect = {
    x: width / 2 - dock_w / 2,
    y: height / 2 - dock_h / 2,
    w: dock_w,
    h: dock_h,
  }
  if try_place_room(grid, 0, dock_rect, rooms, room_id) {
    // Mark docking bay entrance
    let cx = dock_rect.x + dock_w / 2
    let cy = dock_rect.y
    grid.set(cx, cy, 0, @types.Door)
    grid.set(cx, dock_rect.y + dock_h - 1, 0, @types.Start)
    room_id += 1
  }
  // Place rooms on each layer - fewer and smaller at greater depth
  for z in 0..<depth_count {
    let depth_factor = 1.0 - z.to_double() / depth_count.to_double() * 0.5
    let max_rooms = (config.room_density * 10.0 * depth_factor).to_int()
    let min_room_size = 4
    let max_room_size = (8.0 * depth_factor).to_int() + min_room_size
    for _attempt in 0..<(max_rooms * 3) {
      let rw = rng.rand_range(min_room_size, max_room_size + 1)
      let rh = rng.rand_range(min_room_size, max_room_size + 1)
      let rx = rng.rand_range(1, width - rw - 1)
      let ry = rng.rand_range(1, height - rh - 1)
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      if try_place_room(grid, z, rect, rooms, room_id) {
        room_id += 1
        // Check if we have enough rooms for this layer
        let mut count = 0
        for r in rooms {
          // Rough check: room's y coordinate might be on this layer
          ignore(r)
          count += 1
        }
        ignore(count)
      }
    }
  }
  // Connect rooms on same layer with corridors (tubes)
  // For each layer, connect rooms pairwise
  for z in 0..<depth_count {
    // Find rooms on this layer by checking which rooms have floor tiles
    let layer_rooms : Array[@types.Room] = []
    for r in rooms {
      let center = r.rect.center()
      if center.x >= 0 &&
        center.x < width &&
        center.y >= 0 &&
        center.y < height &&
        grid.get(center.x, center.y, z) == @types.Floor {
        layer_rooms.push(r)
      }
    }
    // Connect consecutive rooms
    for i in 0..<layer_rooms.length() {
      if i + 1 < layer_rooms.length() {
        let c1 = layer_rooms[i].rect.center()
        let c2 = layer_rooms[i + 1].rect.center()
        // Carve L-shaped corridor
        let mut x = c1.x
        let mut y = c1.y
        while x != c2.x {
          if grid.get(x, y, z) == @types.Water {
            grid.set(x, y, z, @types.Corridor)
          }
          if x < c2.x {
            x += 1
          } else {
            x -= 1
          }
        }
        while y != c2.y {
          if grid.get(x, y, z) == @types.Water {
            grid.set(x, y, z, @types.Corridor)
          }
          if y < c2.y {
            y += 1
          } else {
            y -= 1
          }
        }
      }
    }
  }
  // Connect layers vertically with airlocks
  for z in 0..<(depth_count - 1) {
    // Find a room center on current layer, place vertical connection
    let cx = width / 2 + rng.rand_range(-3, 4)
    let cy = height / 2 + rng.rand_range(-3, 4)
    // Carve vertical shaft
    for dz in z..=(z + 1) {
      if grid.get(cx, cy, dz) == @types.Water {
        grid.set(cx, cy, dz, @types.Door)
      }
      // Ensure surrounding cells are accessible
      let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      for d in dirs {
        let nx = cx + d.0
        let ny = cy + d.1
        if nx >= 0 && nx < width && ny >= 0 && ny < height {
          if grid.get(nx, ny, dz) == @types.Water {
            grid.set(nx, ny, dz, @types.Corridor)
          }
        }
      }
    }
  }
  { grid, rooms }
}
