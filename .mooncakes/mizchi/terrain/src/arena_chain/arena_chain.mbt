///|
pub(all) struct ArenaChainConfig {
  width : Int
  height : Int
  num_arenas : Int
  arena_min_size : Int
  arena_max_size : Int
  corridor_width : Int
  side_room_chance : Double
  side_room_min_size : Int
  side_room_max_size : Int
  max_placement_attempts : Int
}

///|
pub fn ArenaChainConfig::default() -> ArenaChainConfig {
  {
    width: 60,
    height: 35,
    num_arenas: 5,
    arena_min_size: 7,
    arena_max_size: 13,
    corridor_width: 2,
    side_room_chance: 0.4,
    side_room_min_size: 3,
    side_room_max_size: 5,
    max_placement_attempts: 50,
  }
}

///|
pub(all) struct ArenaChainResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

// Direction pattern for zigzag: right, down, left, down, ...
///|
fn direction_pattern(idx : Int) -> @types.Point {
  match idx % 4 {
    0 => { x: 1, y: 0 } // right
    1 => { x: 0, y: 1 } // down
    2 => { x: -1, y: 0 } // left
    _ => { x: 0, y: 1 } // down
  }
}

///|
pub fn generate(
  config : ArenaChainConfig,
  rng : @types.RNG
) -> ArenaChainResult {
  let rooms : Array[@types.Room] = []
  let edges : Array[@types.Edge] = []
  // Place first arena near center-left
  let first_w = rng.rand_range(config.arena_min_size, config.arena_max_size + 1)
  let first_h = rng.rand_range(config.arena_min_size, config.arena_max_size + 1)
  let first_rect : @types.Rect = {
    x: (config.width / 4 - first_w / 2).clamp(min=1, max=config.width - first_w - 1),
    y: (config.height / 4 - first_h / 2).clamp(
      min=1,
      max=config.height - first_h - 1,
    ),
    w: first_w,
    h: first_h,
  }
  rooms.push({ rect: first_rect, id: 0 })
  // Place remaining arenas in zigzag pattern
  for i in 1..<config.num_arenas {
    let dir = direction_pattern(i - 1)
    let aw = rng.rand_range(config.arena_min_size, config.arena_max_size + 1)
    let ah = rng.rand_range(config.arena_min_size, config.arena_max_size + 1)
    let prev = rooms[i - 1].rect
    let prev_center = prev.center()
    let mut placed = false
    let mut best_rect : @types.Rect = { x: 1, y: 1, w: aw, h: ah }
    for _ in 0..<config.max_placement_attempts {
      let gap = rng.rand_range(3, 7)
      let spread = rng.rand_range(-3, 4)
      let cx = prev_center.x + dir.x * (prev.w / 2 + aw / 2 + gap) + dir.y *
        spread
      let cy = prev_center.y + dir.y * (prev.h / 2 + ah / 2 + gap) + dir.x *
        spread
      let candidate : @types.Rect = {
        x: (cx - aw / 2).clamp(min=1, max=config.width - aw - 1),
        y: (cy - ah / 2).clamp(min=1, max=config.height - ah - 1),
        w: aw,
        h: ah,
      }
      // Check collision with existing rooms (with padding)
      let mut collides = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 2,
          y: existing.rect.y - 2,
          w: existing.rect.w + 4,
          h: existing.rect.h + 4,
        }
        if candidate.intersects(padded) {
          collides = true
          break
        }
      }
      if not(collides) {
        best_rect = candidate
        placed = true
        break
      }
    }
    if not(placed) {
      // Fallback: place with offset from prev
      best_rect = {
        x: (prev_center.x + dir.x * (prev.w / 2 + aw / 2 + 3)).clamp(
          min=1,
          max=config.width - aw - 1,
        ),
        y: (prev_center.y + dir.y * (prev.h / 2 + ah / 2 + 3)).clamp(
          min=1,
          max=config.height - ah - 1,
        ),
        w: aw,
        h: ah,
      }
    }
    rooms.push({ rect: best_rect, id: i })
    edges.push({ from: i - 1, to: i })
  }
  // Add side rooms
  let num_arenas = rooms.length()
  for i in 0..<num_arenas {
    if rng.rand_bool(config.side_room_chance) {
      let sw = rng.rand_range(
        config.side_room_min_size,
        config.side_room_max_size + 1,
      )
      let sh = rng.rand_range(
        config.side_room_min_size,
        config.side_room_max_size + 1,
      )
      let arena = rooms[i].rect
      let arena_c = arena.center()
      let dir = rng.rand_direction()
      let sx = (arena_c.x + dir.x * (arena.w / 2 + sw / 2 + 3)).clamp(
        min=1,
        max=config.width - sw - 1,
      )
      let sy = (arena_c.y + dir.y * (arena.h / 2 + sh / 2 + 3)).clamp(
        min=1,
        max=config.height - sh - 1,
      )
      let side_rect : @types.Rect = { x: sx, y: sy, w: sw, h: sh }
      let mut collides = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if side_rect.intersects(padded) {
          collides = true
          break
        }
      }
      if not(collides) {
        let side_id = rooms.length()
        rooms.push({ rect: side_rect, id: side_id })
        edges.push({ from: i, to: side_id })
      }
    }
  }
  // Render grid
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve rooms
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Carve corridors with width
  for edge in edges {
    let from_room = rooms[edge.from]
    let to_room = rooms[edge.to]
    let from_c = from_room.rect.center()
    let to_c = to_room.rect.center()
    // Carve multiple parallel corridors for width
    for offset in 0..<config.corridor_width {
      let from_offset : @types.Point = { x: from_c.x, y: from_c.y + offset }
      let to_offset : @types.Point = { x: to_c.x, y: to_c.y + offset }
      grid.carve_l_corridor(from_offset, to_offset, @types.Corridor)
    }
  }
  // Place doors at room perimeters
  for room in rooms {
    let rect = room.rect
    for x in rect.x..<(rect.x + rect.w) {
      if rect.y > 0 && grid.get(x, rect.y - 1) == @types.Corridor {
        grid.set(x, rect.y, @types.Door)
      }
      let by = rect.y + rect.h - 1
      if by + 1 < config.height && grid.get(x, by + 1) == @types.Corridor {
        grid.set(x, by, @types.Door)
      }
    }
    for y in rect.y..<(rect.y + rect.h) {
      if rect.x > 0 && grid.get(rect.x - 1, y) == @types.Corridor {
        grid.set(rect.x, y, @types.Door)
      }
      let rx = rect.x + rect.w - 1
      if rx + 1 < config.width && grid.get(rx + 1, y) == @types.Corridor {
        grid.set(rx, y, @types.Door)
      }
    }
  }
  // Place Start in first arena, Goal in last arena
  let start_c = rooms[0].rect.center()
  grid.set(start_c.x, start_c.y, @types.Start)
  let goal_c = rooms[config.num_arenas - 1].rect.center()
  grid.set(goal_c.x, goal_c.y, @types.Goal)
  { grid, rooms, edges }
}
