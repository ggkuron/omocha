///|
pub(all) struct MirrorDungeonConfig {
  width : Int
  height : Int
  num_rooms : Int
  corruption : Double
}

///|
pub fn MirrorDungeonConfig::default() -> MirrorDungeonConfig {
  { width: 50, height: 30, num_rooms: 6, corruption: 0.15 }
}

///|
pub(all) struct MirrorDungeonResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| BSP-like node for the left half
priv struct MirrorBspNode {
  rect : @types.Rect
  left : MirrorBspNode?
  right : MirrorBspNode?
  room : @types.Rect?
}

///| Split a rectangle recursively for BSP room generation
fn bsp_split(
  rect : @types.Rect,
  depth : Int,
  max_depth : Int,
  min_size : Int,
  rng : @types.RNG
) -> MirrorBspNode {
  if depth >= max_depth || (rect.w < min_size * 2 && rect.h < min_size * 2) {
    return { rect, left: None, right: None, room: None }
  }
  let horizontal = if rect.w > rect.h * 2 {
    false
  } else if rect.h > rect.w * 2 {
    true
  } else {
    rng.rand_bool(0.5)
  }
  if horizontal {
    if rect.h < min_size * 2 {
      return { rect, left: None, right: None, room: None }
    }
    let split_y = rng.rand_range(rect.y + min_size, rect.y + rect.h - min_size)
    let top : @types.Rect = { x: rect.x, y: rect.y, w: rect.w, h: split_y - rect.y }
    let bottom : @types.Rect = {
      x: rect.x,
      y: split_y,
      w: rect.w,
      h: rect.y + rect.h - split_y,
    }
    let left = bsp_split(top, depth + 1, max_depth, min_size, rng)
    let right = bsp_split(bottom, depth + 1, max_depth, min_size, rng)
    { rect, left: Some(left), right: Some(right), room: None }
  } else {
    if rect.w < min_size * 2 {
      return { rect, left: None, right: None, room: None }
    }
    let split_x = rng.rand_range(rect.x + min_size, rect.x + rect.w - min_size)
    let left_rect : @types.Rect = {
      x: rect.x,
      y: rect.y,
      w: split_x - rect.x,
      h: rect.h,
    }
    let right_rect : @types.Rect = {
      x: split_x,
      y: rect.y,
      w: rect.x + rect.w - split_x,
      h: rect.h,
    }
    let left = bsp_split(left_rect, depth + 1, max_depth, min_size, rng)
    let right = bsp_split(right_rect, depth + 1, max_depth, min_size, rng)
    { rect, left: Some(left), right: Some(right), room: None }
  }
}

///| Place rooms in BSP leaf nodes
fn bsp_place_rooms(
  node : MirrorBspNode,
  min_size : Int,
  rng : @types.RNG,
  rooms : Array[@types.Room]
) -> MirrorBspNode {
  match (node.left, node.right) {
    (Some(l), Some(r)) => {
      let new_left = bsp_place_rooms(l, min_size, rng, rooms)
      let new_right = bsp_place_rooms(r, min_size, rng, rooms)
      { ..node, left: Some(new_left), right: Some(new_right) }
    }
    _ => {
      let r = node.rect
      if r.w >= min_size && r.h >= min_size {
        let room_w = rng.rand_range(min_size, r.w)
        let room_h = rng.rand_range(min_size, r.h)
        let room_x = rng.rand_range(r.x, r.x + r.w - room_w)
        let room_y = rng.rand_range(r.y, r.y + r.h - room_h)
        let room_rect : @types.Rect = { x: room_x, y: room_y, w: room_w, h: room_h }
        rooms.push({ rect: room_rect, id: rooms.length() })
        { ..node, room: Some(room_rect) }
      } else {
        node
      }
    }
  }
}

///| Get a room center from a BSP node (traversing left-first)
fn bsp_get_center(node : MirrorBspNode) -> @types.Point? {
  match node.room {
    Some(r) => Some(r.center())
    None =>
      match node.left {
        Some(l) => bsp_get_center(l)
        None =>
          match node.right {
            Some(r) => bsp_get_center(r)
            None => None
          }
      }
  }
}

///| Connect rooms in the BSP tree with corridors
fn bsp_connect(node : MirrorBspNode, grid : @types.Grid2D, tile : @types.Tile) -> Unit {
  match (node.left, node.right) {
    (Some(l), Some(r)) => {
      bsp_connect(l, grid, tile)
      bsp_connect(r, grid, tile)
      let c1 = bsp_get_center(l)
      let c2 = bsp_get_center(r)
      match (c1, c2) {
        (Some(p1), Some(p2)) => grid.carve_l_corridor(p1, p2, tile)
        _ => ()
      }
    }
    _ => ()
  }
}

///| Mirror the left half to the right half, using different tiles
fn mirror_half(
  grid : @types.Grid2D,
  half_w : Int,
  full_w : Int,
  corruption : Double,
  rng : @types.RNG
) -> Unit {
  for y in 0..<grid.height {
    for x in 0..<half_w {
      let mirror_x = full_w - 1 - x
      if not(grid.in_bounds(mirror_x, y)) {
        continue
      }
      let tile = grid.get(x, y)
      // Apply corruption: sometimes skip mirroring
      if rng.rand_bool(corruption) {
        continue
      }
      // Map tiles for the mirrored side
      let mirrored_tile = match tile {
        @types.Floor => @types.Corridor // Different tile on mirror side
        @types.Corridor => @types.Floor
        @types.Wall => @types.Wall
        @types.Door => @types.Door
        @types.Empty => @types.Empty
        _ => tile
      }
      grid.set(mirror_x, y, mirrored_tile)
    }
  }
}

///| Create mirrored rooms (reflected across the vertical axis)
fn create_mirrored_rooms(
  original_rooms : Array[@types.Room],
  _half_w : Int,
  full_w : Int
) -> Array[@types.Room] {
  let mirrored : Array[@types.Room] = []
  for room in original_rooms {
    let mirror_x = full_w - (room.rect.x + room.rect.w)
    let mirrored_rect : @types.Rect = {
      x: mirror_x,
      y: room.rect.y,
      w: room.rect.w,
      h: room.rect.h,
    }
    mirrored.push({
      rect: mirrored_rect,
      id: original_rooms.length() + mirrored.length(),
    })
  }
  mirrored
}

///| Carve a vertical corridor down the center connecting both halves
fn carve_central_corridor(
  grid : @types.Grid2D,
  center_x : Int,
  height : Int
) -> Unit {
  // Find connected floor sections and carve corridor between them
  let mut first_floor_y = -1
  let mut last_floor_y = -1
  for y in 0..<height {
    // Check if there are floors nearby on either side
    let has_left = if grid.in_bounds(center_x - 2, y) {
      grid.get(center_x - 2, y) != @types.Wall &&
      grid.get(center_x - 2, y) != @types.Empty
    } else {
      false
    }
    let has_right = if grid.in_bounds(center_x + 2, y) {
      grid.get(center_x + 2, y) != @types.Wall &&
      grid.get(center_x + 2, y) != @types.Empty
    } else {
      false
    }
    if has_left || has_right {
      if first_floor_y == -1 {
        first_floor_y = y
      }
      last_floor_y = y
    }
  }
  // Carve the central corridor
  if first_floor_y >= 0 && last_floor_y >= 0 {
    for y in first_floor_y..=last_floor_y {
      if grid.in_bounds(center_x, y) {
        grid.set(center_x, y, @types.Door)
      }
      // Make corridor 3 tiles wide
      if grid.in_bounds(center_x - 1, y) {
        grid.set(center_x - 1, y, @types.Corridor)
      }
      if grid.in_bounds(center_x + 1, y) {
        grid.set(center_x + 1, y, @types.Corridor)
      }
    }
  }
}

///|
pub fn generate(
  config : MirrorDungeonConfig,
  rng : @types.RNG
) -> MirrorDungeonResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let half_w = config.width / 2 - 1
  // Generate BSP rooms on the left half only
  let left_region : @types.Rect = { x: 1, y: 1, w: half_w, h: config.height - 2 }
  let max_depth = 3
  let min_size = 3
  let left_rooms : Array[@types.Room] = []
  let tree = bsp_split(left_region, 0, max_depth, min_size, rng)
  let tree = bsp_place_rooms(tree, min_size, rng, left_rooms)
  // Limit rooms to num_rooms
  while left_rooms.length() > config.num_rooms {
    ignore(left_rooms.pop())
  }
  // Carve left-side rooms
  for room in left_rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect left-side rooms via BSP
  bsp_connect(tree, grid, @types.Corridor)
  // Mirror the left half to the right half
  mirror_half(grid, half_w + 1, config.width, config.corruption, rng)
  // Create mirrored room entries
  let mirrored_rooms = create_mirrored_rooms(left_rooms, half_w, config.width)
  // Carve central corridor connecting both halves
  let center_x = config.width / 2
  carve_central_corridor(grid, center_x, config.height)
  // Place asymmetric puzzle elements:
  // Key on left side, LockedDoor on right side (or vice versa)
  if left_rooms.length() > 0 && mirrored_rooms.length() > 0 {
    // Place Key in a random left room
    let key_room_idx = rng.next_int(left_rooms.length())
    let key_room = left_rooms[key_room_idx]
    let key_pos = key_room.rect.center()
    grid.set(key_pos.x, key_pos.y, @types.Key)
    // Place LockedDoor in the mirrored room on the right side
    let lock_room = mirrored_rooms[key_room_idx]
    let lock_pos = lock_room.rect.center()
    if grid.in_bounds(lock_pos.x, lock_pos.y) {
      grid.set(lock_pos.x, lock_pos.y, @types.LockedDoor)
    }
  }
  // Place Start on left side, Goal on right side
  if left_rooms.length() > 0 {
    let start_room = left_rooms[0]
    let start_x = start_room.rect.x + 1
    let start_y = start_room.rect.y + 1
    if grid.in_bounds(start_x, start_y) {
      grid.set(start_x, start_y, @types.Start)
    }
  }
  if mirrored_rooms.length() > 0 {
    let goal_room = mirrored_rooms[mirrored_rooms.length() - 1]
    let goal_x = goal_room.rect.x + goal_room.rect.w - 2
    let goal_y = goal_room.rect.y + goal_room.rect.h - 2
    if grid.in_bounds(goal_x, goal_y) {
      grid.set(goal_x, goal_y, @types.Goal)
    }
  }
  // Combine all rooms
  let all_rooms : Array[@types.Room] = []
  for room in left_rooms {
    all_rooms.push(room)
  }
  for room in mirrored_rooms {
    all_rooms.push(room)
  }
  { grid: grid, rooms: all_rooms }
}
