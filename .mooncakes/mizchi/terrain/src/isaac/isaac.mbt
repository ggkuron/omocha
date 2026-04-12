///|
pub(all) struct IsaacConfig {
  width : Int
  height : Int
  room_size : Int
  num_rooms : Int
  min_rooms : Int
  max_dead_ends : Int
}

///|
pub fn IsaacConfig::default() -> IsaacConfig {
  {
    width: 63,
    height: 49,
    room_size: 9,
    num_rooms: 12,
    min_rooms: 8,
    max_dead_ends: 4,
  }
}

///|
pub(all) struct IsaacResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Tracks a room slot in the grid-of-rooms layout.
priv struct RoomSlot {
  gx : Int
  gy : Int
  mut room_type : Int
  // 0=normal, 1=start, 2=boss, 3=treasure, 4=shop
}

///| Phase 1: Random walk to place connected room slots on a grid.
fn place_room_slots(
  grid_cols : Int,
  grid_rows : Int,
  num_rooms : Int,
  min_rooms : Int,
  rng : @types.RNG
) -> Array[RoomSlot] {
  let occupied : Array[Bool] = Array::make(grid_cols * grid_rows, false)
  let slots : Array[RoomSlot] = []
  let center_gx = grid_cols / 2
  let center_gy = grid_rows / 2
  // Place starting room at center
  occupied[center_gy * grid_cols + center_gx] = true
  slots.push({ gx: center_gx, gy: center_gy, room_type: 1 })
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  let target = num_rooms.max(min_rooms)
  // Random walk from existing rooms to add new ones
  for _ in 0..<(target * 20) {
    if slots.length() >= target {
      break
    }
    // Pick a random existing slot
    let src_idx = rng.next_int(slots.length())
    let src = slots[src_idx]
    // Pick a random direction
    let dir_idx = rng.next_int(4)
    let dir = dirs[dir_idx]
    let nx = src.gx + dir.0
    let ny = src.gy + dir.1
    if nx >= 0 && nx < grid_cols && ny >= 0 && ny < grid_rows {
      if not(occupied[ny * grid_cols + nx]) {
        // Count how many neighbors this new cell would have
        let mut neighbor_count = 0
        for d in dirs {
          let ax = nx + d.0
          let ay = ny + d.1
          if ax >= 0 && ax < grid_cols && ay >= 0 && ay < grid_rows {
            if occupied[ay * grid_cols + ax] {
              neighbor_count += 1
            }
          }
        }
        // Only place if it has 1-2 neighbors to avoid overly dense clusters
        if neighbor_count <= 2 {
          occupied[ny * grid_cols + nx] = true
          slots.push({ gx: nx, gy: ny, room_type: 0 })
        }
      }
    }
  }
  slots
}

///| Phase 2: Find dead-end rooms (rooms with exactly one neighbor).
fn find_dead_ends(
  slots : Array[RoomSlot],
  grid_cols : Int,
  grid_rows : Int
) -> Array[Int] {
  let occupied : Array[Bool] = Array::make(grid_cols * grid_rows, false)
  for slot in slots {
    occupied[slot.gy * grid_cols + slot.gx] = true
  }
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  let dead_ends : Array[Int] = []
  for i, slot in slots {
    // Skip the start room
    if slot.room_type == 1 {
      continue
    }
    let mut neighbor_count = 0
    for d in dirs {
      let nx = slot.gx + d.0
      let ny = slot.gy + d.1
      if nx >= 0 && nx < grid_cols && ny >= 0 && ny < grid_rows {
        if occupied[ny * grid_cols + nx] {
          neighbor_count += 1
        }
      }
    }
    if neighbor_count == 1 {
      dead_ends.push(i)
    }
  }
  dead_ends
}

///| Phase 3: Assign special room types to dead ends.
fn assign_special_rooms(
  slots : Array[RoomSlot],
  dead_ends : Array[Int],
  max_dead_ends : Int,
  rng : @types.RNG
) -> Unit {
  if dead_ends.length() == 0 {
    return
  }
  // Shuffle dead ends for random assignment
  let shuffled = dead_ends.copy()
  rng.rand_shuffle(shuffled)
  let limit = max_dead_ends.min(shuffled.length())
  // Assign: boss(2), treasure(3), shop(4)
  let special_types : Array[Int] = [2, 3, 4]
  for i in 0..<limit {
    let slot_idx = shuffled[i]
    if i < special_types.length() {
      slots[slot_idx].room_type = special_types[i]
    }
  }
}

///| Phase 4: Render room slots to the grid with walls, floors, doors.
fn render_isaac_grid(
  slots : Array[RoomSlot],
  grid_cols : Int,
  grid_rows : Int,
  config : IsaacConfig
) -> (@types.Grid2D, Array[@types.Room]) {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Empty)
  let rooms : Array[@types.Room] = []
  let rs = config.room_size
  let occupied : Array[Bool] = Array::make(grid_cols * grid_rows, false)
  for slot in slots {
    occupied[slot.gy * grid_cols + slot.gx] = true
  }
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  for i, slot in slots {
    let px = slot.gx * rs
    let py = slot.gy * rs
    // Fill room border with walls
    for y in py..<(py + rs) {
      for x in px..<(px + rs) {
        if x >= 0 && x < config.width && y >= 0 && y < config.height {
          if x == px || x == px + rs - 1 || y == py || y == py + rs - 1 {
            grid.set(x, y, @types.Wall)
          } else {
            grid.set(x, y, @types.Floor)
          }
        }
      }
    }
    // Place special tiles in center
    let cx = px + rs / 2
    let cy = py + rs / 2
    if cx >= 0 && cx < config.width && cy >= 0 && cy < config.height {
      match slot.room_type {
        1 => grid.set(cx, cy, @types.Start)
        2 => grid.set(cx, cy, @types.Goal)
        3 => grid.set(cx, cy, @types.Key)
        _ => ()
      }
    }
    // Record room (interior only)
    let room_rect : @types.Rect = {
      x: px + 1,
      y: py + 1,
      w: rs - 2,
      h: rs - 2,
    }
    rooms.push({ rect: room_rect, id: i })
    // Place doors to adjacent rooms
    for d in dirs {
      let nx = slot.gx + d.0
      let ny = slot.gy + d.1
      if nx >= 0 && nx < grid_cols && ny >= 0 && ny < grid_rows {
        if occupied[ny * grid_cols + nx] {
          // Place a door on the shared wall
          let door_x = px + rs / 2 + d.0 * (rs / 2)
          let door_y = py + rs / 2 + d.1 * (rs / 2)
          if door_x >= 0 &&
            door_x < config.width &&
            door_y >= 0 &&
            door_y < config.height {
            // Shop rooms get Door only at entrance
            if slot.room_type == 4 {
              grid.set(door_x, door_y, @types.Door)
            } else {
              grid.set(door_x, door_y, @types.Door)
            }
          }
        }
      }
    }
  }
  (grid, rooms)
}

///|
pub fn generate(config : IsaacConfig, rng : @types.RNG) -> IsaacResult {
  let rs = config.room_size
  let grid_cols = config.width / rs
  let grid_rows = config.height / rs
  let slots = place_room_slots(
    grid_cols, grid_rows, config.num_rooms, config.min_rooms, rng,
  )
  let dead_ends = find_dead_ends(slots, grid_cols, grid_rows)
  assign_special_rooms(slots, dead_ends, config.max_dead_ends, rng)
  let (grid, rooms) = render_isaac_grid(slots, grid_cols, grid_rows, config)
  { grid, rooms }
}
