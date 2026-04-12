///|
pub(all) struct RiverConfig {
  width : Int
  height : Int
  num_drops : Int
  noise_scale : Double
  height_threshold : Double
  river_width : Int
  num_sources : Int
}

///|
pub fn RiverConfig::default() -> RiverConfig {
  {
    width: 50,
    height: 25,
    num_drops: 200,
    noise_scale: 0.08,
    height_threshold: 0.5,
    river_width: 1,
    num_sources: 5,
  }
}

///|
pub(all) struct RiverResult {
  grid : @types.Grid2D
  rivers : Int
}

///|
fn hash_noise(x : Int, y : Int, seed : Int) -> Double {
  let mut h = (x * 374761393 + y * 668265263 + seed * 1274126177)
    .reinterpret_as_uint()
  h = (h ^ (h >> 13)) * 1274126177U
  h = h ^ (h >> 16)
  (h.reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
fn value_noise(
  px : Double,
  py : Double,
  seed : Int,
  octaves : Int,
  scale : Double
) -> Double {
  let mut value = 0.0
  let mut amplitude = 1.0
  let mut frequency = scale
  let mut max_amp = 0.0
  for oct = 0; oct < octaves; {
    ignore(oct)
    let sx = px * frequency
    let sy = py * frequency
    let x0 = if sx >= 0.0 {
      sx.to_int()
    } else {
      sx.to_int() - 1
    }
    let y0 = if sy >= 0.0 {
      sy.to_int()
    } else {
      sy.to_int() - 1
    }
    let x1 = x0 + 1
    let y1 = y0 + 1
    let fx = sx - x0.to_double()
    let fy = sy - y0.to_double()
    let tx = fx * fx * (3.0 - 2.0 * fx)
    let ty = fy * fy * (3.0 - 2.0 * fy)
    let n00 = hash_noise(x0, y0, seed + oct)
    let n10 = hash_noise(x1, y0, seed + oct)
    let n01 = hash_noise(x0, y1, seed + oct)
    let n11 = hash_noise(x1, y1, seed + oct)
    let nx0 = n00 + (n10 - n00) * tx
    let nx1 = n01 + (n11 - n01) * tx
    let n = nx0 + (nx1 - nx0) * ty
    value = value + n * amplitude
    max_amp = max_amp + amplitude
    amplitude = amplitude * 0.5
    frequency = frequency * 2.0
    continue oct + 1
  }
  value / max_amp
}

///|
pub fn generate(config : RiverConfig, rng : @types.RNG) -> RiverResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let seed = rng.next_int(100000)
  // Generate heightmap
  let heightmap : Array[Double] = Array::make(w * h, 0.0)
  for y in 0..<h {
    for x in 0..<w {
      let n = value_noise(
        x.to_double(),
        y.to_double(),
        seed,
        4,
        config.noise_scale,
      )
      heightmap[y * w + x] = n
    }
  }
  // Track water flow count per cell
  let flow_count : Array[Int] = Array::make(w * h, 0)
  // Find source positions - pick high points
  let sources : Array[(Int, Int)] = []
  for _s = 0; _s < config.num_sources; {
    let sx = rng.rand_range(1, w - 1)
    let sy = rng.rand_range(1, h - 1)
    // Search for local maximum near this random point
    let mut best_x = sx
    let mut best_y = sy
    let mut best_h = heightmap[sy * w + sx]
    for dy in -3..=3 {
      for dx in -3..=3 {
        let nx = sx + dx
        let ny = sy + dy
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
          let nh = heightmap[ny * w + nx]
          if nh > best_h {
            best_h = nh
            best_x = nx
            best_y = ny
          }
        }
      }
    }
    sources.push((best_x, best_y))
    continue _s + 1
  }
  // Simulate water drops
  for _d = 0; _d < config.num_drops; {
    // Pick a random source
    let src_idx = rng.next_int(sources.length())
    let src = sources[src_idx]
    let mut cx = src.0
    let mut cy = src.1
    // Add some randomness to start position
    cx = cx + rng.rand_range(-2, 3)
    cy = cy + rng.rand_range(-2, 3)
    if cx < 1 {
      cx = 1
    }
    if cx >= w - 1 {
      cx = w - 2
    }
    if cy < 1 {
      cy = 1
    }
    if cy >= h - 1 {
      cy = h - 2
    }
    // Drop follows steepest descent
    for _step = 0; _step < w + h; {
      flow_count[cy * w + cx] = flow_count[cy * w + cx] + 1
      let current_h = heightmap[cy * w + cx]
      let mut min_h = current_h
      let mut next_x = cx
      let mut next_y = cy
      // Check 4 neighbors
      let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      for d in dirs {
        let nx = cx + d.0
        let ny = cy + d.1
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
          let nh = heightmap[ny * w + nx]
          if nh < min_h {
            min_h = nh
            next_x = nx
            next_y = ny
          }
        }
      }
      // Also consider random direction with small chance
      if rng.rand_bool(0.15) {
        let rd = rng.rand_pick(dirs)
        let rx = cx + rd.0
        let ry = cy + rd.1
        if rx >= 0 && rx < w && ry >= 0 && ry < h {
          next_x = rx
          next_y = ry
        }
      }
      if next_x == cx && next_y == cy {
        // Stuck in local minimum - erode slightly and stop
        heightmap[cy * w + cx] = heightmap[cy * w + cx] - 0.01
        break
      }
      cx = next_x
      cy = next_y
      continue _step + 1
    }
    continue _d + 1
  }
  // Convert heightmap and flow to tiles
  let mut river_cells = 0
  for y in 0..<h {
    for x in 0..<w {
      let fc = flow_count[y * w + x]
      let ht = heightmap[y * w + x]
      if fc > 0 {
        // River or corridor based on width
        if fc >= config.river_width * 2 {
          grid.set(x, y, @types.Water)
          // Widen thick rivers
          let dirs2 : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
          for d in dirs2 {
            let nx = x + d.0
            let ny = y + d.1
            if grid.in_bounds(nx, ny) && grid.get(nx, ny) == @types.Wall {
              grid.set(nx, ny, @types.Water)
            }
          }
          river_cells = river_cells + 1
        } else if fc >= 1 {
          grid.set(x, y, @types.Corridor)
          river_cells = river_cells + 1
        }
      } else if ht < config.height_threshold {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Recount actual river/water cells
  let mut total_rivers = 0
  for y in 0..<h {
    for x in 0..<w {
      let t = grid.get(x, y)
      if t == @types.Water || t == @types.Corridor {
        total_rivers = total_rivers + 1
      }
    }
  }
  { grid, rivers: total_rivers }
}
