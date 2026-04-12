///|
pub(all) struct SokobanConfig {
  width : Int
  height : Int
  num_rooms : Int
  blocks_per_room : Int
}

///|
pub fn SokobanConfig::default() -> SokobanConfig {
  { width: 40, height: 30, num_rooms: 6, blocks_per_room: 3 }
}

///|
pub(all) struct SokobanResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Try to place a room without overlapping existing rooms
fn try_place_room(
  rooms : Array[@types.Room],
  config : SokobanConfig,
  rng : @types.RNG
) -> @types.Room? {
  let min_size = 5
  let max_size = 8
  for _ in 0..<60 {
    let rw = rng.rand_range(min_size, max_size + 1)
    let rh = rng.rand_range(min_size, max_size + 1)
    let rx = rng.rand_range(1, config.width - rw - 1)
    let ry = rng.rand_range(1, config.height - rh - 1)
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    let mut overlaps = false
    for existing in rooms {
      let padded : @types.Rect = {
        x: existing.rect.x - 2,
        y: existing.rect.y - 2,
        w: existing.rect.w + 4,
        h: existing.rect.h + 4,
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

///| Connect two rooms with a narrow corridor (width=1)
fn carve_narrow_corridor(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  grid.carve_l_corridor(from, to, @types.Corridor)
}

///| Place wall blocks inside a room as pushable obstacles
fn place_blocks(
  grid : @types.Grid2D,
  room : @types.Room,
  count : Int,
  rng : @types.RNG
) -> Unit {
  let inner_x_min = room.rect.x + 1
  let inner_x_max = room.rect.x + room.rect.w - 2
  let inner_y_min = room.rect.y + 1
  let inner_y_max = room.rect.y + room.rect.h - 2
  if inner_x_max <= inner_x_min || inner_y_max <= inner_y_min {
    return
  }
  let mut placed = 0
  for _ in 0..<(count * 10) {
    if placed >= count {
      break
    }
    let bx = rng.rand_range(inner_x_min, inner_x_max + 1)
    let by = rng.rand_range(inner_y_min, inner_y_max + 1)
    // Don't block the center
    let center = room.rect.center()
    if bx == center.x && by == center.y {
      continue
    }
    if grid.get(bx, by) == @types.Floor {
      grid.set(bx, by, @types.Wall)
      placed += 1
    }
  }
}

///| Place goal tiles in alcoves (corners of rooms)
fn place_goals_in_alcoves(
  grid : @types.Grid2D,
  room : @types.Room,
  count : Int,
  rng : @types.RNG
) -> Unit {
  // Candidate alcove positions: corners of the room interior
  let corners : Array[@types.Point] = [
    { x: room.rect.x + 1, y: room.rect.y + 1 },
    { x: room.rect.x + room.rect.w - 2, y: room.rect.y + 1 },
    { x: room.rect.x + 1, y: room.rect.y + room.rect.h - 2 },
    { x: room.rect.x + room.rect.w - 2, y: room.rect.y + room.rect.h - 2 },
  ]
  rng.rand_shuffle(corners)
  let mut placed = 0
  for corner in corners {
    if placed >= count {
      break
    }
    if grid.in_bounds(corner.x, corner.y) && grid.get(corner.x, corner.y) == @types.Floor {
      grid.set(corner.x, corner.y, @types.Goal)
      placed += 1
    }
  }
}

///|
pub fn generate(config : SokobanConfig, rng : @types.RNG) -> SokobanResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Place rooms
  for _ in 0..<config.num_rooms {
    match try_place_room(rooms, config, rng) {
      Some(room) => rooms.push(room)
      None => ()
    }
  }
  // Carve rooms as floor
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect rooms with narrow corridors
  for i in 0..<(rooms.length() - 1) {
    let from_center = rooms[i].rect.center()
    let to_center = rooms[i + 1].rect.center()
    carve_narrow_corridor(grid, from_center, to_center)
  }
  // Place blocks (pushable wall obstacles) in each room
  for room in rooms {
    place_blocks(grid, room, config.blocks_per_room, rng)
  }
  // Place goal tiles in alcoves of each room
  for room in rooms {
    place_goals_in_alcoves(grid, room, 1, rng)
  }
  // Place start at first room center
  if rooms.length() > 0 {
    let start = rooms[0].rect.center()
    grid.set(start.x, start.y, @types.Start)
  }
  { grid, rooms }
}
