///|
pub(all) struct CrystalConfig {
  width : Int
  height : Int
  num_seeds : Int
  growth_steps : Int
  axis_bias : Double
  connect_chance : Double
  min_crystal_size : Int
}

///|
pub fn CrystalConfig::default() -> CrystalConfig {
  {
    width: 50,
    height: 25,
    num_seeds: 8,
    growth_steps: 100,
    axis_bias: 0.7,
    connect_chance: 0.8,
    min_crystal_size: 4,
  }
}

///|
pub(all) struct CrystalResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn crystal_rng_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///| Axis preference: 0=horizontal, 1=vertical, 2=diagonal-right, 3=diagonal-left
priv struct Seed {
  x : Int
  y : Int
  axis : Int
  id : Int
}

///|
pub fn generate(config : CrystalConfig, rng : @types.RNG) -> CrystalResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Track which crystal owns each cell (-1 = none)
  let owner : Array[Int] = Array::make(w * h, -1)
  // Place seeds
  let seeds : Array[Seed] = []
  for i = 0; i < config.num_seeds; {
    let sx = rng.rand_range(3, w - 3)
    let sy = rng.rand_range(3, h - 3)
    let axis = rng.next_int(4)
    let seed : Seed = { x: sx, y: sy, axis, id: i }
    seeds.push(seed)
    grid.set(sx, sy, @types.Floor)
    owner[sy * w + sx] = i
    continue i + 1
  }
  // Growth frontier for each crystal
  let frontiers : Array[Array[(Int, Int)]] = []
  for i = 0; i < config.num_seeds; {
    ignore(i)
    frontiers.push([])
    continue i + 1
  }
  // Initialize frontiers
  for s in seeds {
    let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for d in dirs {
      let nx = s.x + d.0
      let ny = s.y + d.1
      if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 {
        frontiers[s.id].push((nx, ny))
      }
    }
  }
  // Growth loop
  for _step = 0; _step < config.growth_steps; {
    for si = 0; si < seeds.length(); {
      let seed = seeds[si]
      let frontier = frontiers[seed.id]
      if frontier.length() == 0 {
        continue si + 1
      }
      // Try to grow from a random frontier cell
      let fi = rng.next_int(frontier.length())
      let cell = frontier[fi]
      let cx = cell.0
      let cy = cell.1
      // Remove from frontier
      ignore(frontier.remove(fi))
      // Skip if already owned
      if owner[cy * w + cx] >= 0 {
        continue si + 1
      }
      // Compute growth probability based on axis preference
      let dx = cx - seed.x
      let dy = cy - seed.y
      let prob = match seed.axis {
        0 => {
          // Horizontal preference
          let adx = if dx < 0 { -dx } else { dx }
          let ady = if dy < 0 { -dy } else { dy }
          if adx >= ady { config.axis_bias } else { 1.0 - config.axis_bias }
        }
        1 => {
          // Vertical preference
          let adx = if dx < 0 { -dx } else { dx }
          let ady = if dy < 0 { -dy } else { dy }
          if ady >= adx { config.axis_bias } else { 1.0 - config.axis_bias }
        }
        2 => {
          // Diagonal (top-left to bottom-right)
          let adx = if dx < 0 { -dx } else { dx }
          let ady = if dy < 0 { -dy } else { dy }
          let diff = if adx > ady { adx - ady } else { ady - adx }
          if diff <= 1 { config.axis_bias } else { 1.0 - config.axis_bias }
        }
        _ => {
          // Diagonal (top-right to bottom-left)
          let adx = if dx < 0 { -dx } else { dx }
          let ady = if dy < 0 { -dy } else { dy }
          let diff = if adx > ady { adx - ady } else { ady - adx }
          if diff <= 1 { config.axis_bias } else { 1.0 - config.axis_bias }
        }
      }
      let r = crystal_rng_double(rng)
      if r < prob {
        // Grow into this cell
        grid.set(cx, cy, @types.Floor)
        owner[cy * w + cx] = seed.id
        // Add neighbors to frontier
        let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for d in dirs {
          let nx = cx + d.0
          let ny = cy + d.1
          if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 && owner[ny * w +
          nx] < 0 {
            frontier.push((nx, ny))
          }
        }
      }
      continue si + 1
    }
    continue _step + 1
  }
  // Compute bounding boxes for each crystal
  let min_x : Array[Int] = Array::make(config.num_seeds, w)
  let max_x : Array[Int] = Array::make(config.num_seeds, 0)
  let min_y : Array[Int] = Array::make(config.num_seeds, h)
  let max_y : Array[Int] = Array::make(config.num_seeds, 0)
  let cell_count : Array[Int] = Array::make(config.num_seeds, 0)
  for y in 0..<h {
    for x in 0..<w {
      let o = owner[y * w + x]
      if o >= 0 {
        cell_count[o] = cell_count[o] + 1
        if x < min_x[o] {
          min_x[o] = x
        }
        if x > max_x[o] {
          max_x[o] = x
        }
        if y < min_y[o] {
          min_y[o] = y
        }
        if y > max_y[o] {
          max_y[o] = y
        }
      }
    }
  }
  // Build rooms from crystals that are large enough
  let rooms : Array[@types.Room] = []
  for i = 0; i < config.num_seeds; {
    if cell_count[i] >= config.min_crystal_size {
      let rect : @types.Rect = {
        x: min_x[i],
        y: min_y[i],
        w: max_x[i] - min_x[i] + 1,
        h: max_y[i] - min_y[i] + 1,
      }
      let room : @types.Room = { rect, id: rooms.length() }
      rooms.push(room)
    }
    continue i + 1
  }
  // Connect adjacent crystals with corridors
  for i = 0; i < rooms.length(); {
    for j = i + 1; j < rooms.length(); {
      let r = crystal_rng_double(rng)
      if r < config.connect_chance {
        let c1 = rooms[i].rect.center()
        let c2 = rooms[j].rect.center()
        grid.carve_l_corridor(c1, c2, @types.Corridor)
      }
      continue j + 1
    }
    continue i + 1
  }
  { grid, rooms }
}
