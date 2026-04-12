///|
pub(all) struct TunnelerConfig {
  width : Int
  height : Int
  num_tunnelers : Int
  tunnel_length : Int
  initial_width : Int
  max_width : Int
  turn_chance : Double
  width_change_chance : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn TunnelerConfig::default() -> TunnelerConfig {
  {
    width: 60,
    height: 30,
    num_tunnelers: 3,
    tunnel_length: 80,
    initial_width: 1,
    max_width: 4,
    turn_chance: 0.2,
    width_change_chance: 0.1,
    room_chance: 0.06,
    room_min_size: 3,
    room_max_size: 7,
  }
}

///|
pub(all) struct TunnelerResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
priv struct TunnelerAgent {
  mut x : Int
  mut y : Int
  mut dx : Int
  mut dy : Int
  mut corridor_width : Int
  mut turn_chance : Double
  mut room_chance : Double
}

///|
pub fn generate(config : TunnelerConfig, rng : @types.RNG) -> TunnelerResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  let cx = config.width / 2
  let cy = config.height / 2
  // Create tunnelers from center with different directions
  let agents : Array[TunnelerAgent] = []
  let dirs : Array[(Int, Int)] = [(1, 0), (-1, 0), (0, 1), (0, -1)]
  for i in 0..<config.num_tunnelers {
    let d = dirs[i % dirs.length()]
    agents.push({
      x: cx,
      y: cy,
      dx: d.0,
      dy: d.1,
      corridor_width: config.initial_width,
      turn_chance: config.turn_chance,
      room_chance: config.room_chance,
    })
  }
  // Run all tunnelers
  for agent in agents {
    for _ in 0..<config.tunnel_length {
      // Carve at current position with current width
      carve_wide(grid, agent.x, agent.y, agent.corridor_width)
      // Maybe spawn a room
      if rng.rand_bool(agent.room_chance) {
        let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rx = (agent.x - rw / 2).clamp(min=1, max=config.width - rw - 1)
        let ry = (agent.y - rh / 2).clamp(min=1, max=config.height - rh - 1)
        let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
        // Increase room chance as agent moves farther
        let rc = agent.room_chance * 1.05
        agent.room_chance = if rc > 0.3 { 0.3 } else { rc }
      }
      // Maybe change width (evolving parameter)
      if rng.rand_bool(config.width_change_chance) {
        if rng.rand_bool(0.6) {
          agent.corridor_width = (agent.corridor_width + 1).min(config.max_width)
        } else {
          agent.corridor_width = (agent.corridor_width - 1).max(1)
        }
      }
      // Maybe turn (evolving parameter)
      if rng.rand_bool(agent.turn_chance) {
        if agent.dx != 0 {
          agent.dx = 0
          agent.dy = if rng.rand_bool(0.5) { 1 } else { -1 }
        } else {
          agent.dy = 0
          agent.dx = if rng.rand_bool(0.5) { 1 } else { -1 }
        }
        // Increase turn chance gradually
        let tc = agent.turn_chance * 1.02
        agent.turn_chance = if tc > 0.5 { 0.5 } else { tc }
      }
      // Advance
      agent.x = (agent.x + agent.dx).clamp(min=2, max=config.width - 3)
      agent.y = (agent.y + agent.dy).clamp(min=2, max=config.height - 3)
    }
  }
  // Place Start at center, Goal at farthest
  grid.set(cx, cy, @types.Floor)
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
        if grid.get(nx, ny) == @types.Wall {
          grid.set(nx, ny, @types.Corridor)
        }
      }
    }
  }
}
