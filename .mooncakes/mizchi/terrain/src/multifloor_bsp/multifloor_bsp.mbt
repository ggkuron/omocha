///|
pub(all) struct MultifloorBspConfig {
  width : Int
  height : Int
  depth : Int
  min_room_size : Int
  stair_count : Int
}

///|
pub fn MultifloorBspConfig::default() -> MultifloorBspConfig {
  { width: 40, height: 20, depth: 3, min_room_size: 4, stair_count: 2 }
}

///|
pub(all) struct MultifloorBspResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
priv struct BspNode {
  rect : @types.Rect
  left : BspNode?
  right : BspNode?
  room : @types.Rect?
}

///|
fn split_node(
  rect : @types.Rect,
  depth : Int,
  max_depth : Int,
  min_size : Int,
  rng : @types.RNG
) -> BspNode {
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
    let top : @types.Rect = {
      x: rect.x,
      y: rect.y,
      w: rect.w,
      h: split_y - rect.y,
    }
    let bottom : @types.Rect = {
      x: rect.x,
      y: split_y,
      w: rect.w,
      h: rect.y + rect.h - split_y,
    }
    let left = split_node(top, depth + 1, max_depth, min_size, rng)
    let right = split_node(bottom, depth + 1, max_depth, min_size, rng)
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
    let left = split_node(left_rect, depth + 1, max_depth, min_size, rng)
    let right = split_node(right_rect, depth + 1, max_depth, min_size, rng)
    { rect, left: Some(left), right: Some(right), room: None }
  }
}

///|
fn place_rooms(
  node : BspNode,
  min_size : Int,
  rng : @types.RNG,
  rooms : Array[@types.Room],
  room_id_offset : Int
) -> BspNode {
  match (node.left, node.right) {
    (Some(l), Some(r)) => {
      let new_left = place_rooms(l, min_size, rng, rooms, room_id_offset)
      let new_right = place_rooms(r, min_size, rng, rooms, room_id_offset)
      { ..node, left: Some(new_left), right: Some(new_right) }
    }
    _ => {
      let r = node.rect
      if r.w >= min_size && r.h >= min_size {
        let room_w = rng.rand_range(min_size, r.w)
        let room_h = rng.rand_range(min_size, r.h)
        let room_x = rng.rand_range(r.x, r.x + r.w - room_w)
        let room_y = rng.rand_range(r.y, r.y + r.h - room_h)
        let room_rect : @types.Rect = {
          x: room_x,
          y: room_y,
          w: room_w,
          h: room_h,
        }
        let room : @types.Room = {
          rect: room_rect,
          id: room_id_offset + rooms.length(),
        }
        rooms.push(room)
        { ..node, room: Some(room_rect) }
      } else {
        node
      }
    }
  }
}

///|
fn get_room_center(node : BspNode) -> @types.Point? {
  match node.room {
    Some(r) => Some(r.center())
    None =>
      match node.left {
        Some(l) => get_room_center(l)
        None =>
          match node.right {
            Some(r) => get_room_center(r)
            None => None
          }
      }
  }
}

///|
fn connect_rooms(node : BspNode, grid : @types.Grid2D) -> Unit {
  match (node.left, node.right) {
    (Some(l), Some(r)) => {
      connect_rooms(l, grid)
      connect_rooms(r, grid)
      let c1 = get_room_center(l)
      let c2 = get_room_center(r)
      match (c1, c2) {
        (Some(p1), Some(p2)) => grid.carve_l_corridor(p1, p2, @types.Corridor)
        _ => ()
      }
    }
    _ => ()
  }
}

///|
pub fn generate(
  config : MultifloorBspConfig,
  rng : @types.RNG
) -> MultifloorBspResult {
  let grid = @types.Grid3D::make(config.width, config.height, config.depth, @types.Wall)
  let all_rooms : Array[@types.Room] = []
  // Per-layer room lists for stair placement
  let layer_rooms : Array[Array[@types.Room]] = []
  // Generate BSP dungeon on each layer
  for z in 0..<config.depth {
    let layer = grid.get_layer(z)
    let full_rect : @types.Rect = {
      x: 1,
      y: 1,
      w: config.width - 2,
      h: config.height - 2,
    }
    let rooms : Array[@types.Room] = []
    let max_bsp_depth = 4
    let tree = split_node(
      full_rect, 0, max_bsp_depth, config.min_room_size, rng,
    )
    let tree = place_rooms(tree, config.min_room_size, rng, rooms, all_rooms.length())
    for room in rooms {
      layer.fill_rect(room.rect, @types.Floor)
    }
    connect_rooms(tree, layer)
    layer_rooms.push(rooms)
    for room in rooms {
      all_rooms.push(room)
    }
  }
  // Place stairs (Door tiles) connecting adjacent layers
  for z in 0..<(config.depth - 1) {
    let upper_rooms = layer_rooms[z]
    let lower_rooms = layer_rooms[z + 1]
    if upper_rooms.length() == 0 || lower_rooms.length() == 0 {
      continue
    }
    let mut placed = 0
    let mut attempts = 0
    let max_attempts = config.stair_count * 20
    while placed < config.stair_count && attempts < max_attempts {
      attempts += 1
      // Pick a random room from the upper layer
      let room = rng.rand_pick(upper_rooms)
      let sx = rng.rand_range(room.rect.x + 1, room.rect.x + room.rect.w - 1)
      let sy = rng.rand_range(room.rect.y + 1, room.rect.y + room.rect.h - 1)
      // Check that corresponding position on the lower layer is floor
      if grid.get(sx, sy, z) == @types.Floor &&
        grid.get(sx, sy, z + 1) == @types.Floor {
        grid.set(sx, sy, z, @types.Door)
        grid.set(sx, sy, z + 1, @types.Door)
        placed += 1
      }
    }
    // Fallback: if no stairs placed, force one at a room center
    if placed == 0 {
      let room_a = rng.rand_pick(upper_rooms)
      let ca = room_a.rect.center()
      grid.set(ca.x, ca.y, z, @types.Door)
      // Also carve floor on lower layer at that position
      grid.set(ca.x, ca.y, z + 1, @types.Door)
    }
  }
  { grid, rooms: all_rooms }
}
