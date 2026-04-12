///|
pub(all) struct RingConfig {
  width : Int
  height : Int
  ring_padding : Int
  num_rooms : Int
  room_min_size : Int
  room_max_size : Int
  num_shortcuts : Int
  shortcut_width : Int
}

///|
pub fn RingConfig::default() -> RingConfig {
  {
    width: 50,
    height: 25,
    ring_padding: 4,
    num_rooms: 8,
    room_min_size: 4,
    room_max_size: 7,
    num_shortcuts: 2,
    shortcut_width: 1,
  }
}

///|
pub(all) struct RingResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn carve_horizontal(grid : @types.Grid2D, y : Int, x1 : Int, x2 : Int, tile : @types.Tile) -> Unit {
  let start = if x1 < x2 { x1 } else { x2 }
  let end = if x1 > x2 { x1 } else { x2 }
  for x in start..=end {
    if grid.in_bounds(x, y) {
      grid.set(x, y, tile)
    }
  }
}

///|
fn carve_vertical(grid : @types.Grid2D, x : Int, y1 : Int, y2 : Int, tile : @types.Tile) -> Unit {
  let start = if y1 < y2 { y1 } else { y2 }
  let end = if y1 > y2 { y1 } else { y2 }
  for y in start..=end {
    if grid.in_bounds(x, y) {
      grid.set(x, y, tile)
    }
  }
}

///|
pub fn generate(config : RingConfig, rng : @types.RNG) -> RingResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Define ring boundaries
  let ring_left = config.ring_padding
  let ring_right = config.width - config.ring_padding - 1
  let ring_top = config.ring_padding
  let ring_bottom = config.height - config.ring_padding - 1
  // Carve the ring corridor (rectangular loop)
  // Top edge
  carve_horizontal(grid, ring_top, ring_left, ring_right, @types.Corridor)
  // Bottom edge
  carve_horizontal(grid, ring_bottom, ring_left, ring_right, @types.Corridor)
  // Left edge
  carve_vertical(grid, ring_left, ring_top, ring_bottom, @types.Corridor)
  // Right edge
  carve_vertical(grid, ring_right, ring_top, ring_bottom, @types.Corridor)
  // Collect ring perimeter points for room placement
  // We'll place rooms branching off at intervals along the ring
  let ring_points : Array[@types.Point] = []
  // Top edge points (excluding corners)
  for x in (ring_left + 2)..<(ring_right - 1) {
    ring_points.push({ x, y: ring_top })
  }
  // Bottom edge points
  for x in (ring_left + 2)..<(ring_right - 1) {
    ring_points.push({ x, y: ring_bottom })
  }
  // Left edge points
  for y in (ring_top + 2)..<(ring_bottom - 1) {
    ring_points.push({ x: ring_left, y })
  }
  // Right edge points
  for y in (ring_top + 2)..<(ring_bottom - 1) {
    ring_points.push({ x: ring_right, y })
  }
  // Shuffle and pick room attachment points
  rng.rand_shuffle(ring_points)
  let num_rooms = if config.num_rooms < ring_points.length() {
    config.num_rooms
  } else {
    ring_points.length()
  }
  for i in 0..<num_rooms {
    let attach = ring_points[i]
    let room_w = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let room_h = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    // Determine which side of the ring this point is on and place room accordingly
    let placement = if attach.y == ring_top {
      // Top edge - place room above
      (attach.x - room_w / 2, ring_top - room_h - 1, attach.x, ring_top - 1)
    } else if attach.y == ring_bottom {
      // Bottom edge - place room below
      (attach.x - room_w / 2, ring_bottom + 2, attach.x, ring_bottom + 1)
    } else if attach.x == ring_left {
      // Left edge - place room to the left
      (ring_left - room_w - 1, attach.y - room_h / 2, ring_left - 1, attach.y)
    } else {
      // Right edge - place room to the right
      (ring_right + 2, attach.y - room_h / 2, ring_right + 1, attach.y)
    }
    let room_x = placement.0
    let room_y = placement.1
    let door_x = placement.2
    let door_y = placement.3
    // Check bounds
    if room_x < 1 ||
      room_y < 1 ||
      room_x + room_w >= config.width - 1 ||
      room_y + room_h >= config.height - 1 {
      continue
    }
    // Check overlap with existing rooms
    let room_rect : @types.Rect = { x: room_x, y: room_y, w: room_w, h: room_h }
    let mut overlaps = false
    for existing in rooms {
      if room_rect.intersects(existing.rect) {
        overlaps = true
        break
      }
    }
    if overlaps {
      continue
    }
    // Carve the room
    grid.fill_rect(room_rect, @types.Floor)
    // Place door at entrance
    if grid.in_bounds(door_x, door_y) {
      grid.set(door_x, door_y, @types.Door)
    }
    // Carve corridor from door to attachment point if needed
    if door_x != attach.x || door_y != attach.y {
      grid.carve_line(
        { x: door_x, y: door_y },
        { x: attach.x, y: attach.y },
        @types.Corridor,
      )
    }
    rooms.push({ rect: room_rect, id: rooms.length() })
  }
  // Add shortcut corridors across the ring (Dark Souls style)
  for _ in 0..<config.num_shortcuts {
    // Pick a random direction for the shortcut (horizontal or vertical)
    if rng.rand_bool(0.5) {
      // Horizontal shortcut: connect left wall to right wall
      let y = rng.rand_range(ring_top + 2, ring_bottom - 1)
      for w in 0..<config.shortcut_width {
        let sy = y + w
        if sy < ring_bottom {
          carve_horizontal(grid, sy, ring_left, ring_right, @types.Corridor)
        }
      }
    } else {
      // Vertical shortcut: connect top wall to bottom wall
      let x = rng.rand_range(ring_left + 2, ring_right - 1)
      for w in 0..<config.shortcut_width {
        let sx = x + w
        if sx < ring_right {
          carve_vertical(grid, sx, ring_top, ring_bottom, @types.Corridor)
        }
      }
    }
  }
  // Place Start and Goal on opposite sides of the ring
  grid.set(ring_left, ring_top, @types.Start)
  grid.set(ring_right, ring_bottom, @types.Goal)
  { grid, rooms }
}
