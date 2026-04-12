///|
pub(all) struct CastleGenConfig {
  width : Int
  height : Int
  max_height : Int
  tower_count : Int
  wall_height : Int
}

///|
pub fn CastleGenConfig::default() -> CastleGenConfig {
  { width: 48, height: 48, max_height: 8, tower_count: 4, wall_height: 4 }
}

///|
pub(all) struct CastleGenResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : CastleGenConfig,
  rng : @types.RNG
) -> CastleGenResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Floor, config.max_height)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Ground level everything at height 0
  for y in 0..<h {
    for x in 0..<w {
      grid.set_height(x, y, 0)
    }
  }
  // Outer wall perimeter with margin
  let outer_margin = 2
  let outer_x = outer_margin
  let outer_y = outer_margin
  let outer_w = w - outer_margin * 2
  let outer_h = h - outer_margin * 2
  // Draw outer walls
  draw_wall_rect(grid, outer_x, outer_y, outer_w, outer_h, config.wall_height)
  // Fill outside of outer wall with Empty
  for y in 0..<h {
    for x in 0..<w {
      if x < outer_x || x >= outer_x + outer_w || y < outer_y ||
        y >= outer_y + outer_h {
        grid.set(x, y, @types.Empty)
        grid.set_height(x, y, 0)
      }
    }
  }
  // Inner courtyard
  let court_margin = 4
  let court_x = outer_x + court_margin
  let court_y = outer_y + court_margin
  let court_w = outer_w - court_margin * 2
  let court_h = outer_h - court_margin * 2
  if court_w > 4 && court_h > 4 {
    // Courtyard is floor at ground level (already set)
    rooms.push(
      { rect: { x: court_x, y: court_y, w: court_w, h: court_h }, id: room_id },
    )
    room_id += 1
    // Inner courtyard walls
    draw_wall_rect(grid, court_x, court_y, court_w, court_h, config.wall_height -
      1)
    // Courtyard interior is floor
    for y in (court_y + 1)..<(court_y + court_h - 1) {
      for x in (court_x + 1)..<(court_x + court_w - 1) {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, 1) // Slightly raised courtyard
      }
    }
    // Gate in outer wall (south side)
    let gate_x = outer_x + outer_w / 2
    let gate_y = outer_y + outer_h - 1
    if grid.in_bounds(gate_x, gate_y) {
      grid.set(gate_x, gate_y, @types.Door)
      grid.set_height(gate_x, gate_y, 0)
      if grid.in_bounds(gate_x - 1, gate_y) {
        grid.set(gate_x - 1, gate_y, @types.Door)
        grid.set_height(gate_x - 1, gate_y, 0)
      }
    }
    // Gate in inner wall (south side)
    let inner_gate_x = court_x + court_w / 2
    let inner_gate_y = court_y + court_h - 1
    if grid.in_bounds(inner_gate_x, inner_gate_y) {
      grid.set(inner_gate_x, inner_gate_y, @types.Door)
      grid.set_height(inner_gate_x, inner_gate_y, 1)
    }
  }
  // Place towers at corners of outer wall
  let tower_radius = 2
  let tower_height = config.max_height
  let tower_positions : Array[(Int, Int)] = [
    (outer_x, outer_y), // top-left
    (outer_x + outer_w - 1, outer_y), // top-right
    (outer_x, outer_y + outer_h - 1), // bottom-left
    (outer_x + outer_w - 1, outer_y + outer_h - 1), // bottom-right
  ]
  // If tower_count > 4, add towers along walls
  let extra_tower_positions : Array[(Int, Int)] = []
  if config.tower_count > 4 {
    let extra = config.tower_count - 4
    for i in 0..<extra {
      let side = i % 4
      let t = (i / 4 + 1).to_double() / ((extra / 4 + 2).to_double())
      match side {
        0 => {
          // top wall
          let tx = outer_x + (outer_w.to_double() * t).to_int()
          extra_tower_positions.push((tx, outer_y))
        }
        1 => {
          // bottom wall
          let tx = outer_x + (outer_w.to_double() * t).to_int()
          extra_tower_positions.push((tx, outer_y + outer_h - 1))
        }
        2 => {
          // left wall
          let ty = outer_y + (outer_h.to_double() * t).to_int()
          extra_tower_positions.push((outer_x, ty))
        }
        _ => {
          // right wall
          let ty = outer_y + (outer_h.to_double() * t).to_int()
          extra_tower_positions.push((outer_x + outer_w - 1, ty))
        }
      }
    }
  }
  // Build corner towers
  let actual_tower_count = tower_positions.length().min(config.tower_count)
  for i in 0..<actual_tower_count {
    let pos = tower_positions[i]
    build_tower(grid, pos.0, pos.1, tower_radius, tower_height, w, h)
    rooms.push(
      {
        rect: {
          x: (pos.0 - tower_radius).max(0),
          y: (pos.1 - tower_radius).max(0),
          w: tower_radius * 2 + 1,
          h: tower_radius * 2 + 1,
        },
        id: room_id,
      },
    )
    room_id += 1
  }
  // Build extra towers
  for pos in extra_tower_positions {
    build_tower(grid, pos.0, pos.1, tower_radius, tower_height, w, h)
    rooms.push(
      {
        rect: {
          x: (pos.0 - tower_radius).max(0),
          y: (pos.1 - tower_radius).max(0),
          w: tower_radius * 2 + 1,
          h: tower_radius * 2 + 1,
        },
        id: room_id,
      },
    )
    room_id += 1
  }
  // Build keep (central building) in the courtyard
  if court_w > 8 && court_h > 8 {
    let keep_w = court_w / 3
    let keep_h = court_h / 3
    let keep_x = court_x + (court_w - keep_w) / 2
    let keep_y = court_y + (court_h - keep_h) / 2
    let keep_height = config.wall_height + 1
    // Keep walls
    draw_wall_rect(grid, keep_x, keep_y, keep_w, keep_h, keep_height)
    // Keep interior
    for y in (keep_y + 1)..<(keep_y + keep_h - 1) {
      for x in (keep_x + 1)..<(keep_x + keep_w - 1) {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, keep_height - 1)
      }
    }
    // Keep door
    let keep_door_x = keep_x + keep_w / 2
    let keep_door_y = keep_y + keep_h - 1
    if grid.in_bounds(keep_door_x, keep_door_y) {
      grid.set(keep_door_x, keep_door_y, @types.Door)
      grid.set_height(keep_door_x, keep_door_y, 1)
    }
    rooms.push(
      { rect: { x: keep_x, y: keep_y, w: keep_w, h: keep_h }, id: room_id },
    )
    room_id += 1
  }
  // Add some rooms in the space between outer and inner walls
  add_wall_rooms(grid, config, rng, outer_x, outer_y, outer_w, outer_h, court_x,
    court_y, court_w, court_h, rooms, room_id)
  // Place Start at gate, Goal in keep
  place_start_goal_castle(grid, w, h)
  { grid, rooms }
}

///|
fn draw_wall_rect(
  grid : @types.HeightGrid,
  rx : Int,
  ry : Int,
  rw : Int,
  rh : Int,
  wall_h : Int
) -> Unit {
  // Top and bottom walls
  for x in rx..<(rx + rw) {
    if grid.in_bounds(x, ry) {
      grid.set(x, ry, @types.Wall)
      grid.set_height(x, ry, wall_h)
    }
    if grid.in_bounds(x, ry + rh - 1) {
      grid.set(x, ry + rh - 1, @types.Wall)
      grid.set_height(x, ry + rh - 1, wall_h)
    }
  }
  // Left and right walls
  for y in ry..<(ry + rh) {
    if grid.in_bounds(rx, y) {
      grid.set(rx, y, @types.Wall)
      grid.set_height(rx, y, wall_h)
    }
    if grid.in_bounds(rx + rw - 1, y) {
      grid.set(rx + rw - 1, y, @types.Wall)
      grid.set_height(rx + rw - 1, y, wall_h)
    }
  }
}

///|
fn build_tower(
  grid : @types.HeightGrid,
  cx : Int,
  cy : Int,
  radius : Int,
  tower_h : Int,
  w : Int,
  h : Int
) -> Unit {
  for dy in -radius..=radius {
    for dx in -radius..=radius {
      let px = cx + dx
      let py = cy + dy
      if px >= 0 && px < w && py >= 0 && py < h {
        // Tower walls on perimeter, floor inside
        if dx == -radius || dx == radius || dy == -radius || dy == radius {
          grid.set(px, py, @types.Wall)
          grid.set_height(px, py, tower_h)
        } else {
          grid.set(px, py, @types.Floor)
          grid.set_height(px, py, tower_h - 1)
        }
      }
    }
  }
}

///|
fn add_wall_rooms(
  grid : @types.HeightGrid,
  config : CastleGenConfig,
  rng : @types.RNG,
  outer_x : Int,
  outer_y : Int,
  outer_w : Int,
  _outer_h : Int,
  court_x : Int,
  court_y : Int,
  _court_w : Int,
  _court_h : Int,
  rooms : Array[@types.Room],
  start_room_id : Int
) -> Unit {
  // Add small rooms (guard rooms) in the wall corridor area
  let corridor_width = court_x - outer_x - 1
  if corridor_width < 3 {
    return
  }
  // Place 2-4 guard rooms
  let num_guard_rooms = rng.rand_range(2, 5)
  let mut cur_room_id = start_room_id
  for _ in 0..<num_guard_rooms {
    let side = rng.next_int(4)
    let mut rx = 0
    let mut ry = 0
    let rw = 3
    let rh = 3
    match side {
      0 => {
        // top corridor
        rx = rng.rand_range(outer_x + 2, outer_x + outer_w - rw - 2)
        ry = outer_y + 1
      }
      1 => {
        // left corridor
        rx = outer_x + 1
        ry = rng.rand_range(outer_y + 2, outer_y + outer_w - rh - 2)
      }
      2 => {
        // right corridor
        rx = court_x + (court_x - outer_x) - 1
        ry = rng.rand_range(outer_y + 2, outer_y + outer_w - rh - 2)
      }
      _ => {
        // bottom corridor
        rx = rng.rand_range(outer_x + 2, outer_x + outer_w - rw - 2)
        ry = court_y + (court_y - outer_y) - 1
      }
    }
    // Clamp positions
    rx = rx.max(outer_x + 1).min(config.width - rw - 1)
    ry = ry.max(outer_y + 1).min(config.height - rh - 1)
    // Place guard room
    for dy in 0..<rh {
      for dx in 0..<rw {
        let px = rx + dx
        let py = ry + dy
        if grid.in_bounds(px, py) {
          grid.set(px, py, @types.Floor)
          grid.set_height(px, py, 2)
        }
      }
    }
    rooms.push({ rect: { x: rx, y: ry, w: rw, h: rh }, id: cur_room_id })
    cur_room_id += 1
  }
}

///|
fn place_start_goal_castle(
  grid : @types.HeightGrid,
  w : Int,
  h : Int
) -> Unit {
  // Start: near the gate (bottom center)
  let mut start_placed = false
  for y = h - 2; y > h / 2; y = y - 1 {
    for x in (w / 2 - 2)..<(w / 2 + 3) {
      if not(start_placed) && x >= 0 && x < w && grid.in_bounds(x, y) {
        let tile = grid.get(x, y)
        if tile == @types.Floor || tile == @types.Door {
          grid.set(x, y, @types.Start)
          start_placed = true
        }
      }
    }
  }
  // Fallback start
  if not(start_placed) {
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        if not(start_placed) && grid.get(x, y) == @types.Floor {
          grid.set(x, y, @types.Start)
          start_placed = true
        }
      }
    }
  }
  // Goal: in the keep (center of map, high floor)
  let mut goal_placed = false
  let mut best_h = -1
  let mut best_x = w / 2
  let mut best_y = h / 2
  for y in (h / 3)..<(h * 2 / 3) {
    for x in (w / 3)..<(w * 2 / 3) {
      if grid.get(x, y) == @types.Floor && grid.get_height(x, y) > best_h {
        best_h = grid.get_height(x, y)
        best_x = x
        best_y = y
      }
    }
  }
  if grid.get(best_x, best_y) != @types.Start {
    grid.set(best_x, best_y, @types.Goal)
    goal_placed = true
  }
  if not(goal_placed) {
    for y = h - 2; y > 0; y = y - 1 {
      for x = w - 2; x > 0; x = x - 1 {
        if not(goal_placed) && grid.get(x, y) == @types.Floor {
          grid.set(x, y, @types.Goal)
          goal_placed = true
        }
      }
    }
  }
}
