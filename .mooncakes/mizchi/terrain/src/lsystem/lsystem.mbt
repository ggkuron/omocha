///|
pub(all) struct LsystemConfig {
  width : Int
  height : Int
  iterations : Int
  segment_length : Int
  turn_angle : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn LsystemConfig::default() -> LsystemConfig {
  {
    width: 50,
    height: 25,
    iterations: 4,
    segment_length: 2,
    turn_angle: 2,
    room_chance: 0.15,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct LsystemResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

// Turtle state for L-system interpretation
///|
priv struct TurtleState {
  mut x : Int
  mut y : Int
  mut dir : Int // 0=right, 1=down, 2=left, 3=up
}

// Direction vectors: 0=right, 1=down, 2=left, 3=up
///|
fn dir_dx(dir : Int) -> Int {
  match dir % 4 {
    0 => 1
    1 => 0
    2 => -1
    3 => 0
    _ => 0
  }
}

///|
fn dir_dy(dir : Int) -> Int {
  match dir % 4 {
    0 => 0
    1 => 1
    2 => 0
    3 => -1
    _ => 0
  }
}

///|
fn expand_lsystem(iterations : Int) -> String {
  // Rule: F -> F[+F]F[-F]F (branching corridors)
  // We represent the string as a StringBuilder and expand char by char
  let current = StringBuilder::new()
  current.write_char('F')
  let mut current_str = current.to_string()
  for _ in 0..<iterations {
    let next = StringBuilder::new()
    let chars = current_str.to_array()
    for c in chars {
      if c == 'F' {
        next.write_string("F[+F]F[-F]F")
      } else {
        next.write_char(c)
      }
    }
    current_str = next.to_string()
    // Limit string length to prevent explosion
    if current_str.to_array().length() > 10000 {
      break
    }
  }
  current_str
}

///|
pub fn generate(
  config : LsystemConfig,
  rng : @types.RNG
) -> LsystemResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Expand L-system string
  let lstr = expand_lsystem(config.iterations)
  let chars = lstr.to_array()
  // Turtle starts at center facing right
  let turtle : TurtleState = {
    x: config.width / 2,
    y: config.height / 2,
    dir: 0,
  }
  // Stack for push/pop
  let stack_x : Array[Int] = []
  let stack_y : Array[Int] = []
  let stack_dir : Array[Int] = []
  // Track branch points for room placement
  let branch_points : Array[@types.Point] = []
  // Interpret the L-system string
  for c in chars {
    match c {
      'F' => {
        // Move forward, carving corridor
        let dx = dir_dx(turtle.dir)
        let dy = dir_dy(turtle.dir)
        for _ in 0..<config.segment_length {
          if grid.in_bounds(turtle.x, turtle.y) {
            grid.set(turtle.x, turtle.y, @types.Corridor)
          }
          turtle.x = turtle.x + dx
          turtle.y = turtle.y + dy
        }
        // Clamp to grid bounds
        if turtle.x < 0 {
          turtle.x = 0
        }
        if turtle.x >= config.width {
          turtle.x = config.width - 1
        }
        if turtle.y < 0 {
          turtle.y = 0
        }
        if turtle.y >= config.height {
          turtle.y = config.height - 1
        }
      }
      '+' => {
        // Turn right
        turtle.dir = (turtle.dir + config.turn_angle) % 4
      }
      '-' => {
        // Turn left
        turtle.dir = (turtle.dir - config.turn_angle + 4) % 4
      }
      '[' => {
        // Push state
        stack_x.push(turtle.x)
        stack_y.push(turtle.y)
        stack_dir.push(turtle.dir)
        // Record branch point
        branch_points.push({ x: turtle.x, y: turtle.y })
      }
      ']' => {
        // Pop state
        if stack_x.length() > 0 {
          turtle.x = stack_x.pop().unwrap()
          turtle.y = stack_y.pop().unwrap()
          turtle.dir = stack_dir.pop().unwrap()
        }
      }
      _ => ()
    }
  }
  // Place rooms at branch points with room_chance
  let mut room_id = 0
  for bp in branch_points {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    // Center room on branch point
    let rx = bp.x - rw / 2
    let ry = bp.y - rh / 2
    // Check bounds
    if rx < 1 || ry < 1 || rx + rw >= config.width - 1 || ry + rh >= config.height -
    1 {
      continue
    }
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    // Carve room
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: room_id })
    room_id += 1
  }
  { grid, rooms }
}
