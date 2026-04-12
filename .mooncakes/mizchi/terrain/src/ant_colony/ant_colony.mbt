///|
pub(all) struct AntColonyConfig {
  width : Int
  height : Int
  num_ants : Int
  steps_per_ant : Int
  pheromone_strength : Double
  evaporation_rate : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn AntColonyConfig::default() -> AntColonyConfig {
  {
    width: 50,
    height: 25,
    num_ants: 10,
    steps_per_ant: 200,
    pheromone_strength: 1.0,
    evaporation_rate: 0.1,
    room_chance: 0.1,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct AntColonyResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn rand_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
pub fn generate(
  config : AntColonyConfig,
  rng : @types.RNG
) -> AntColonyResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let pheromone : Array[Double] = Array::make(w * h, 0.0)
  let visit_count : Array[Int] = Array::make(w * h, 0)
  // Run each ant
  for _ant in 0..<config.num_ants {
    // Random start position (avoid edges)
    let mut ax = rng.rand_range(1, w - 1)
    let mut ay = rng.rand_range(1, h - 1)
    grid.set(ax, ay, @types.Floor)
    pheromone[ay * w + ax] = pheromone[ay * w + ax] + config.pheromone_strength
    visit_count[ay * w + ax] = visit_count[ay * w + ax] + 1
    for _step in 0..<config.steps_per_ant {
      // Gather pheromone weights for 4 directions
      let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      let weights : Array[Double] = []
      let valid_dirs : Array[(Int, Int)] = []
      for d in dirs {
        let nx = ax + d.0
        let ny = ay + d.1
        if nx > 0 && nx < w - 1 && ny > 0 && ny < h - 1 {
          // Base weight 1.0 + pheromone at destination
          let weight = 1.0 + pheromone[ny * w + nx]
          weights.push(weight)
          valid_dirs.push(d)
        }
      }
      if valid_dirs.length() == 0 {
        // Stuck, skip
        continue
      }
      // Compute total weight
      let mut total = 0.0
      for wt in weights {
        total = total + wt
      }
      // Pick direction by weighted random
      let mut roll = rand_double(rng) * total
      let mut chosen = 0
      for i in 0..<weights.length() {
        roll = roll - weights[i]
        if roll <= 0.0 {
          chosen = i
          break
        }
      }
      let dir = valid_dirs[chosen]
      ax = ax + dir.0
      ay = ay + dir.1
      grid.set(ax, ay, @types.Floor)
      pheromone[ay * w + ax] = pheromone[ay * w + ax] + config.pheromone_strength
      visit_count[ay * w + ax] = visit_count[ay * w + ax] + 1
    }
    // Evaporate pheromones after each ant
    for i in 0..<(w * h) {
      pheromone[i] = pheromone[i] * (1.0 - config.evaporation_rate)
    }
  }
  // Find high-traffic intersections and optionally place rooms
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for cy in 2..<(h - 2) {
    for cx in 2..<(w - 2) {
      if visit_count[cy * w + cx] > config.num_ants * 2 {
        if rng.rand_bool(config.room_chance) {
          let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
          let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
          let rx = cx - rw / 2
          let ry = cy - rh / 2
          // Check bounds
          if rx >= 1 && ry >= 1 && rx + rw < w - 1 && ry + rh < h - 1 {
            // Check no overlap with existing rooms
            let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
            let mut overlaps = false
            for r in rooms {
              if r.rect.intersects(rect) {
                overlaps = true
                break
              }
            }
            if not(overlaps) {
              grid.fill_rect(rect, @types.Floor)
              rooms.push({ rect, id: room_id })
              room_id = room_id + 1
            }
          }
        }
      }
    }
  }
  { grid, rooms }
}
