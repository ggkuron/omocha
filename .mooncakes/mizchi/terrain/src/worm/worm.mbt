///|
pub(all) struct WormConfig {
  width : Int
  height : Int
  num_worms : Int
  worm_length : Int
  worm_min_width : Int
  worm_max_width : Int
  turn_chance : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn WormConfig::default() -> WormConfig {
  {
    width: 50,
    height: 25,
    num_worms: 4,
    worm_length: 60,
    worm_min_width: 1,
    worm_max_width: 3,
    turn_chance: 0.3,
    room_chance: 0.08,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct WormResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : WormConfig, rng : @types.RNG) -> WormResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  let cx = config.width / 2
  let cy = config.height / 2
  // Start all worms from center
  grid.set(cx, cy, @types.Floor)
  for _ in 0..<config.num_worms {
    let mut x = cx
    let mut y = cy
    let mut dx = 0
    let mut dy = 0
    // Pick initial random direction
    let init_dir = rng.next_int(4)
    match init_dir {
      0 => { dx = 0; dy = -1 }
      1 => { dx = 0; dy = 1 }
      2 => { dx = -1; dy = 0 }
      _ => { dx = 1; dy = 0 }
    }
    let width = rng.rand_range(config.worm_min_width, config.worm_max_width + 1)
    for _ in 0..<config.worm_length {
      // Carve with width
      carve_wide(grid, x, y, width)
      // Maybe place a room
      if rng.rand_bool(config.room_chance) {
        let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rx = (x - rw / 2).clamp(min=1, max=config.width - rw - 1)
        let ry = (y - rh / 2).clamp(min=1, max=config.height - rh - 1)
        let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
      // Maybe turn
      if rng.rand_bool(config.turn_chance) {
        let turn = rng.next_int(4)
        match turn {
          0 => { dx = 0; dy = -1 }
          1 => { dx = 0; dy = 1 }
          2 => { dx = -1; dy = 0 }
          _ => { dx = 1; dy = 0 }
        }
      }
      // Advance
      x = (x + dx).clamp(min=1, max=config.width - 2)
      y = (y + dy).clamp(min=1, max=config.height - 2)
    }
  }
  // Place Start at center, Goal at farthest point
  grid.set(cx, cy, @types.Start)
  let filled = @types.flood_fill(grid, cx, cy)
  let mut max_dist = 0
  let mut goal : @types.Point = { x: cx, y: cy }
  for p in filled {
    let dist = abs(p.x - cx) + abs(p.y - cy)
    if dist > max_dist {
      max_dist = dist
      goal = p
    }
  }
  grid.set(goal.x, goal.y, @types.Goal)
  { grid, rooms }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn carve_wide(grid : @types.Grid2D, x : Int, y : Int, width : Int) -> Unit {
  let half = width / 2
  for dy in -half..=half {
    for dx in -half..=half {
      let nx = x + dx
      let ny = y + dy
      if grid.in_bounds(nx, ny) && nx > 0 && ny > 0 && nx < grid.width - 1 &&
        ny < grid.height - 1 {
        grid.set(nx, ny, @types.Floor)
      }
    }
  }
}
