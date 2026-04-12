///|
pub(all) struct CliffDungeonConfig {
  width : Int
  height : Int
  max_height : Int
  cliff_chance : Double
  room_count : Int
}

///|
pub fn CliffDungeonConfig::default() -> CliffDungeonConfig {
  { width: 48, height: 48, max_height: 6, cliff_chance: 0.4, room_count: 8 }
}

///|
pub(all) struct CliffDungeonResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : CliffDungeonConfig,
  rng : @types.RNG
) -> CliffDungeonResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Wall, config.max_height)
  let rooms : Array[@types.Room] = []
  // Set all walls to height 0 initially
  for y in 0..<h {
    for x in 0..<w {
      grid.set_height(x, y, 0)
    }
  }
  // Place rooms at various heights
  let min_room_size = 4
  let max_room_size = 9
  for i in 0..<config.room_count {
    let mut placed = false
    let mut attempts = 0
    while not(placed) && attempts < 100 {
      attempts += 1
      let rw = rng.rand_range(min_room_size, max_room_size + 1)
      let rh = rng.rand_range(min_room_size, max_room_size + 1)
      let rx = rng.rand_range(1, w - rw - 1)
      let ry = rng.rand_range(1, h - rh - 1)
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      // Check no overlap with existing rooms (with 1-cell buffer)
      let mut overlaps = false
      for r in rooms {
        let expanded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(expanded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        // Assign a height level for this room
        let room_height = rng.rand_range(0, config.max_height)
        // Carve room
        for dy in 0..<rh {
          for dx in 0..<rw {
            let px = rx + dx
            let py = ry + dy
            grid.set(px, py, @types.Floor)
            grid.set_height(px, py, room_height)
          }
        }
        rooms.push({ rect, id: i })
        placed = true
      }
    }
  }
  // Connect rooms with corridors
  // Sort rooms by id (already in order) and connect each to the next
  for i in 0..<(rooms.length() - 1) {
    let from_center = rooms[i].rect.center()
    let to_center = rooms[i + 1].rect.center()
    let from_h = grid.get_height(from_center.x, from_center.y)
    let to_h = grid.get_height(to_center.x, to_center.y)
    // Carve L-shaped corridor with height gradient
    carve_height_corridor(grid, from_center, to_center, from_h, to_h)
  }
  // Add cliff faces: walls near height transitions become tall cliff walls
  add_cliff_faces(grid, config, rng)
  // Place Start in first room, Goal in last room
  if rooms.length() > 0 {
    let start_c = rooms[0].rect.center()
    grid.set(start_c.x, start_c.y, @types.Start)
    let goal_c = rooms[rooms.length() - 1].rect.center()
    grid.set(goal_c.x, goal_c.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_height_corridor(
  grid : @types.HeightGrid,
  from : @types.Point,
  to : @types.Point,
  from_h : Int,
  to_h : Int
) -> Unit {
  // Horizontal segment
  let mut x = from.x
  let y = from.y
  let total_dx = abs(to.x - from.x) + abs(to.y - from.y)
  let mut steps = 0
  while x != to.x {
    if grid.in_bounds(x, y) {
      let t = if total_dx > 0 {
        steps.to_double() / total_dx.to_double()
      } else {
        0.0
      }
      let cur_h = from_h + ((to_h - from_h).to_double() * t).to_int()
      if grid.get(x, y) == @types.Wall {
        grid.set(x, y, @types.Corridor)
      }
      grid.set_height(x, y, cur_h)
    }
    if x < to.x {
      x += 1
    } else {
      x -= 1
    }
    steps += 1
  }
  // Vertical segment
  let mut vy = from.y
  while vy != to.y {
    if grid.in_bounds(x, vy) {
      let t = if total_dx > 0 {
        steps.to_double() / total_dx.to_double()
      } else {
        1.0
      }
      let cur_h = from_h + ((to_h - from_h).to_double() * t).to_int()
      if grid.get(x, vy) == @types.Wall {
        grid.set(x, vy, @types.Corridor)
      }
      grid.set_height(x, vy, cur_h)
    }
    if vy < to.y {
      vy += 1
    } else {
      vy -= 1
    }
    steps += 1
  }
  // Final cell
  if grid.in_bounds(to.x, to.y) {
    if grid.get(to.x, to.y) == @types.Wall {
      grid.set(to.x, to.y, @types.Corridor)
    }
    grid.set_height(to.x, to.y, to_h)
  }
}

///|
fn add_cliff_faces(
  grid : @types.HeightGrid,
  config : CliffDungeonConfig,
  rng : @types.RNG
) -> Unit {
  let w = config.width
  let h = config.height
  // For each wall cell adjacent to a floor/corridor cell with significant height,
  // raise the wall to create a cliff face
  for y in 0..<h {
    for x in 0..<w {
      if grid.get(x, y) == @types.Wall {
        let mut max_neighbor_h = 0
        let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) {
            let tile = grid.get(nx, ny)
            if tile == @types.Floor || tile == @types.Corridor {
              let nh = grid.get_height(nx, ny)
              if nh > max_neighbor_h {
                max_neighbor_h = nh
              }
            }
          }
        }
        if max_neighbor_h > 0 && rng.rand_bool(config.cliff_chance) {
          // Raise wall to be higher than adjacent floor (cliff face)
          let cliff_h = (max_neighbor_h + rng.rand_range(1, 3)).min(
            config.max_height,
          )
          grid.set_height(x, y, cliff_h)
        }
      }
    }
  }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}
