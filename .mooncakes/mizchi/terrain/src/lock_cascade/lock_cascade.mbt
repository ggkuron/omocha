///|
pub(all) struct LockCascadeConfig {
  width : Int
  height : Int
  chain_length : Int
  side_room_chance : Double
}

///|
pub fn LockCascadeConfig::default() -> LockCascadeConfig {
  { width: 60, height: 30, chain_length: 6, side_room_chance: 0.3 }
}

///|
pub(all) struct LockCascadeResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Compute progressively larger room sizes for each chain position
fn compute_room_size(
  chain_index : Int,
  chain_length : Int,
  rng : @types.RNG
) -> (Int, Int) {
  // Base size grows with chain index
  let base = 4 + chain_index * 2 / chain_length
  let min_size = base + 2
  let max_size = base + 5
  let w = rng.rand_range(min_size, max_size + 1)
  let h = rng.rand_range(min_size, max_size + 1)
  (w, h)
}

///| Place the main chain of rooms left-to-right across the map
fn place_chain_rooms(
  config : LockCascadeConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  // Divide width into segments for each chain room
  let segment_w = (config.width - 4) / config.chain_length
  for i in 0..<config.chain_length {
    let (rw, rh) = compute_room_size(i, config.chain_length, rng)
    let actual_w = if rw > segment_w - 2 { segment_w - 2 } else { rw }
    let actual_h = if rh > config.height - 4 {
      config.height - 4
    } else {
      rh
    }
    let seg_x = 2 + i * segment_w
    let rx = rng.rand_range(seg_x, seg_x + segment_w - actual_w)
    let ry = rng.rand_range(2, config.height - actual_h - 2)
    rooms.push({ rect: { x: rx, y: ry, w: actual_w, h: actual_h }, id: i })
  }
  rooms
}

///| Place a side room branching off a main room
fn try_place_side_room(
  parent : @types.Room,
  rooms : Array[@types.Room],
  config : LockCascadeConfig,
  rng : @types.RNG
) -> @types.Room? {
  let sw = rng.rand_range(3, 5)
  let sh = rng.rand_range(3, 5)
  // Try to place above or below the parent room
  let directions : Array[Int] = [0, 1]
  rng.rand_shuffle(directions)
  for dir in directions {
    let (sx, sy) = if dir == 0 {
      // Above
      (parent.rect.x + rng.rand_range(0, parent.rect.w - sw), parent.rect.y - sh - 1)
    } else {
      // Below
      (parent.rect.x + rng.rand_range(0, parent.rect.w - sw), parent.rect.y + parent.rect.h + 1)
    }
    if sx < 1 || sy < 1 || sx + sw >= config.width - 1 || sy + sh >= config.height - 1 {
      continue
    }
    let rect : @types.Rect = { x: sx, y: sy, w: sw, h: sh }
    let mut overlaps = false
    for existing in rooms {
      let padded : @types.Rect = {
        x: existing.rect.x - 1,
        y: existing.rect.y - 1,
        w: existing.rect.w + 2,
        h: existing.rect.h + 2,
      }
      if rect.intersects(padded) {
        overlaps = true
        break
      }
    }
    if not(overlaps) {
      return Some({ rect, id: rooms.length() })
    }
  }
  None
}

///| Carve corridor with a LockedDoor at the entrance to the next room
fn carve_locked_corridor(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  let mid : @types.Point = { x: to.x, y: from.y }
  grid.carve_line(from, mid, @types.Corridor)
  grid.carve_line(mid, to, @types.Corridor)
  // Place LockedDoor one step before the destination
  let lock_x = if mid.x < to.x {
    to.x - 1
  } else if mid.x > to.x {
    to.x + 1
  } else if mid.y < to.y {
    to.x
  } else {
    to.x
  }
  let lock_y = if mid.x == to.x {
    if mid.y < to.y {
      to.y - 1
    } else {
      to.y + 1
    }
  } else {
    to.y
  }
  if grid.in_bounds(lock_x, lock_y) {
    grid.set(lock_x, lock_y, @types.LockedDoor)
  }
}

///|
pub fn generate(
  config : LockCascadeConfig,
  rng : @types.RNG
) -> LockCascadeResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms = place_chain_rooms(config, rng)
  // Track side rooms separately for connecting
  let side_rooms : Array[(@types.Room, Int)] = [] // (side_room, parent_index)
  // Try to add side rooms
  for i in 0..<rooms.length() {
    if rng.rand_bool(config.side_room_chance) {
      match try_place_side_room(rooms[i], rooms, config, rng) {
        Some(side) => {
          side_rooms.push((side, i))
          rooms.push(side)
        }
        None => ()
      }
    }
  }
  // Carve all rooms onto the grid
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect chain rooms with locked corridors
  // Room N has the key for room N+1's lock
  let chain_len = if config.chain_length > rooms.length() {
    rooms.length()
  } else {
    config.chain_length
  }
  for i in 0..<(chain_len - 1) {
    let from_center = rooms[i].rect.center()
    let to_center = rooms[i + 1].rect.center()
    carve_locked_corridor(grid, from_center, to_center)
    // Place key for next room's lock inside current room
    let key_x = rng.rand_range(rooms[i].rect.x + 1, rooms[i].rect.x + rooms[i].rect.w - 1)
    let key_y = rng.rand_range(rooms[i].rect.y + 1, rooms[i].rect.y + rooms[i].rect.h - 1)
    if grid.in_bounds(key_x, key_y) && grid.get(key_x, key_y) == @types.Floor {
      grid.set(key_x, key_y, @types.Key)
    }
  }
  // Connect side rooms to their parent with regular corridors
  for pair in side_rooms {
    let side = pair.0
    let parent_idx = pair.1
    let from_center = rooms[parent_idx].rect.center()
    let to_center = side.rect.center()
    grid.carve_l_corridor(from_center, to_center, @types.Corridor)
    // Place Goal (treasure) in side rooms
    grid.set(to_center.x, to_center.y, @types.Goal)
  }
  // Place Start in first room
  if rooms.length() > 0 {
    let start = rooms[0].rect.center()
    grid.set(start.x, start.y, @types.Start)
  }
  // Place Goal in last chain room
  if chain_len > 0 {
    let goal = rooms[chain_len - 1].rect.center()
    grid.set(goal.x, goal.y, @types.Goal)
  }
  { grid, rooms }
}
