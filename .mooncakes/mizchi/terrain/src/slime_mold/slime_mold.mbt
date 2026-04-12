///|
pub(all) struct SlimeMoldConfig {
  width : Int
  height : Int
  num_agents : Int
  num_food : Int
  steps : Int
  sensor_angle : Int
  sensor_distance : Int
  deposit_amount : Double
  decay_rate : Double
  threshold : Double
}

///|
pub fn SlimeMoldConfig::default() -> SlimeMoldConfig {
  {
    width: 50,
    height: 25,
    num_agents: 200,
    num_food: 5,
    steps: 300,
    sensor_angle: 1,
    sensor_distance: 3,
    deposit_amount: 1.0,
    decay_rate: 0.1,
    threshold: 0.5,
  }
}

///|
pub(all) struct SlimeMoldResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn slime_rng_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///| Direction lookup table (8 directions)
/// Index 0=right, 1=upper-right, 2=up, 3=upper-left, 4=left, 5=lower-left, 6=down, 7=lower-right
fn dir_dx(d : Int) -> Int {
  // dx: 1, 1, 0, -1, -1, -1, 0, 1
  match d {
    0 => 1
    1 => 1
    2 => 0
    3 => -1
    4 => -1
    5 => -1
    6 => 0
    7 => 1
    _ => 0
  }
}

///|
fn dir_dy(d : Int) -> Int {
  // dy: 0, -1, -1, -1, 0, 1, 1, 1
  match d {
    0 => 0
    1 => -1
    2 => -1
    3 => -1
    4 => 0
    5 => 1
    6 => 1
    7 => 1
    _ => 0
  }
}

///|
priv struct SlimeAgent {
  mut x : Int
  mut y : Int
  mut heading : Int
}

///|
fn sense_trail(
  trail : Array[Double],
  w : Int,
  h : Int,
  x : Int,
  y : Int,
  heading : Int,
  offset : Int,
  distance : Int
) -> Double {
  let d = (heading + offset + 8) % 8
  let sx = x + dir_dx(d) * distance
  let sy = y + dir_dy(d) * distance
  if sx >= 0 && sx < w && sy >= 0 && sy < h {
    trail[sy * w + sx]
  } else {
    0.0
  }
}

///|
pub fn generate(
  config : SlimeMoldConfig,
  rng : @types.RNG
) -> SlimeMoldResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let trail : Array[Double] = Array::make(w * h, 0.0)
  // Place food sources and deposit initial pheromone
  let food_positions : Array[(Int, Int)] = []
  for _f = 0; _f < config.num_food; {
    let fx = rng.rand_range(3, w - 3)
    let fy = rng.rand_range(3, h - 3)
    food_positions.push((fx, fy))
    // Deposit large trail around food
    for dy in -2..=2 {
      for dx in -2..=2 {
        let nx = fx + dx
        let ny = fy + dy
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
          trail[ny * w + nx] = trail[ny * w + nx] + 5.0
        }
      }
    }
    continue _f + 1
  }
  // Create agents at center
  let center_x = w / 2
  let center_y = h / 2
  let agents : Array[SlimeAgent] = []
  for _a = 0; _a < config.num_agents; {
    let ax = center_x + rng.rand_range(-3, 4)
    let ay = center_y + rng.rand_range(-3, 4)
    let ah = rng.next_int(8)
    let agent : SlimeAgent = { x: ax, y: ay, heading: ah }
    agents.push(agent)
    continue _a + 1
  }
  // Simulation loop
  for _step = 0; _step < config.steps; {
    // Move each agent
    for agent in agents {
      // Sense
      let front = sense_trail(
        trail,
        w,
        h,
        agent.x,
        agent.y,
        agent.heading,
        0,
        config.sensor_distance,
      )
      let left = sense_trail(
        trail,
        w,
        h,
        agent.x,
        agent.y,
        agent.heading,
        -config.sensor_angle,
        config.sensor_distance,
      )
      let right = sense_trail(
        trail,
        w,
        h,
        agent.x,
        agent.y,
        agent.heading,
        config.sensor_angle,
        config.sensor_distance,
      )
      // Turn toward highest pheromone
      if front >= left && front >= right {
        // Keep heading (or small random)
        if rng.rand_bool(0.1) {
          if rng.rand_bool(0.5) {
            agent.heading = (agent.heading + 1) % 8
          } else {
            agent.heading = (agent.heading + 7) % 8
          }
        }
      } else if left > right {
        agent.heading = (agent.heading + 8 - config.sensor_angle) % 8
      } else if right > left {
        agent.heading = (agent.heading + config.sensor_angle) % 8
      } else {
        // Equal left and right, random turn
        if rng.rand_bool(0.5) {
          agent.heading = (agent.heading + config.sensor_angle) % 8
        } else {
          agent.heading = (agent.heading + 8 - config.sensor_angle) % 8
        }
      }
      // Move forward
      let nx = agent.x + dir_dx(agent.heading)
      let ny = agent.y + dir_dy(agent.heading)
      if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 {
        agent.x = nx
        agent.y = ny
      } else {
        // Bounce: reverse direction
        agent.heading = (agent.heading + 4) % 8
      }
      // Deposit pheromone
      if agent.x >= 0 && agent.x < w && agent.y >= 0 && agent.y < h {
        trail[agent.y * w + agent.x] = trail[agent.y * w + agent.x] +
          config.deposit_amount
      }
    }
    // Decay trail
    for i in 0..<(w * h) {
      trail[i] = trail[i] * (1.0 - config.decay_rate)
    }
    // Diffuse trail (simple blur)
    if _step % 5 == 0 {
      let old_trail = trail.copy()
      for y in 1..<(h - 1) {
        for x in 1..<(w - 1) {
          let sum = old_trail[(y - 1) * w + x] +
            old_trail[(y + 1) * w + x] +
            old_trail[y * w + (x - 1)] +
            old_trail[y * w + (x + 1)]
          trail[y * w + x] = (old_trail[y * w + x] + sum * 0.25) / 2.0
        }
      }
    }
    continue _step + 1
  }
  // Find max trail value for normalization
  let mut max_trail = 0.0
  for i in 0..<(w * h) {
    if trail[i] > max_trail {
      max_trail = trail[i]
    }
  }
  if max_trail < 0.001 {
    max_trail = 1.0
  }
  // Convert trail to tiles
  for y in 0..<h {
    for x in 0..<w {
      let normalized = trail[y * w + x] / max_trail
      if normalized > config.threshold {
        grid.set(x, y, @types.Floor)
      } else if normalized > config.threshold * 0.5 {
        grid.set(x, y, @types.Corridor)
      }
    }
  }
  // Place rooms at food sources
  let rooms : Array[@types.Room] = []
  for fp in food_positions {
    let fx = fp.0
    let fy = fp.1
    let room_size = 3
    let half = room_size / 2
    let rx = fx - half
    let ry = fy - half
    let rect : @types.Rect = { x: rx, y: ry, w: room_size, h: room_size }
    grid.fill_rect(rect, @types.Floor)
    let room : @types.Room = { rect, id: rooms.length() }
    rooms.push(room)
  }
  // Add small random perturbation with a final pass
  ignore(slime_rng_double(rng))
  { grid, rooms }
}
