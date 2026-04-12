///|
pub(all) struct LightningConfig {
  width : Int
  height : Int
  num_bolts : Int
  branch_chance : Double
  max_steps : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn LightningConfig::default() -> LightningConfig {
  {
    width: 50,
    height: 25,
    num_bolts: 3,
    branch_chance: 0.3,
    max_steps: 500,
    room_chance: 0.15,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct LightningResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn lightning_rng_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
pub fn generate(
  config : LightningConfig,
  rng : @types.RNG
) -> LightningResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Track bolt membership per cell
  let bolt_map : Array[Bool] = Array::make(w * h, false)
  // Track how many times each cell is part of a bolt path (for thickness)
  let thickness : Array[Int] = Array::make(w * h, 0)
  let rooms : Array[@types.Room] = []
  // Generate multiple bolts
  for _b = 0; _b < config.num_bolts; {
    // Start point: top half, random x
    let start_x = rng.rand_range(w / 4, 3 * w / 4)
    let start_y = rng.rand_range(0, h / 4)
    // Target point: bottom half
    let target_x = rng.rand_range(w / 4, 3 * w / 4)
    let target_y = rng.rand_range(3 * h / 4, h)
    // Use DLA-like growth from start toward target
    // Active tips that can grow
    let tips : Array[(Int, Int)] = [(start_x, start_y)]
    bolt_map[start_y * w + start_x] = true
    thickness[start_y * w + start_x] = thickness[start_y * w + start_x] + 1
    grid.set(start_x, start_y, @types.Corridor)
    for _s = 0; _s < config.max_steps; {
      if tips.length() == 0 {
        break
      }
      // Pick a random tip
      let ti = rng.next_int(tips.length())
      let tip = tips[ti]
      let tx = tip.0
      let ty = tip.1
      // Find best neighbor to grow into
      let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      // Score each direction
      let mut best_score = -1.0
      let mut best_x = tx
      let mut best_y = ty
      for d in dirs {
        let nx = tx + d.0
        let ny = ty + d.1
        if nx >= 0 && nx < w && ny >= 0 && ny < h && not(bolt_map[ny * w + nx]) {
          // Score: distance to target (inverted) + randomness
          let ddx = (target_x - nx).to_double()
          let ddy = (target_y - ny).to_double()
          let dist = ddx * ddx + ddy * ddy
          // Lower dist = higher score. Add random factor
          let rand_factor = lightning_rng_double(rng) * 100.0
          let score = 1.0 / (dist + 1.0) * 1000.0 + rand_factor
          if score > best_score {
            best_score = score
            best_x = nx
            best_y = ny
          }
        }
      }
      if best_x == tx && best_y == ty {
        // This tip is stuck, remove it
        ignore(tips.remove(ti))
        continue _s + 1
      }
      // Grow bolt
      bolt_map[best_y * w + best_x] = true
      thickness[best_y * w + best_x] = thickness[best_y * w + best_x] + 1
      grid.set(best_x, best_y, @types.Corridor)
      // Update this tip's position
      tips[ti] = (best_x, best_y)
      // Branch chance
      if rng.rand_bool(config.branch_chance) {
        // Add a new tip at the same position (it will diverge next step)
        tips.push((best_x, best_y))
      }
      // Room placement at branch points
      if tips.length() > 1 && rng.rand_bool(config.room_chance) {
        let rsize = rng.rand_range(config.room_min_size, config.room_max_size + 1)
        let rx = best_x - rsize / 2
        let ry = best_y - rsize / 2
        // Clamp
        let clamped_rx = if rx < 0 { 0 } else { rx }
        let clamped_ry = if ry < 0 { 0 } else { ry }
        let clamped_rw = if clamped_rx + rsize > w {
          w - clamped_rx
        } else {
          rsize
        }
        let clamped_rh = if clamped_ry + rsize > h {
          h - clamped_ry
        } else {
          rsize
        }
        if clamped_rw >= config.room_min_size &&
          clamped_rh >= config.room_min_size {
          let rect : @types.Rect = {
            x: clamped_rx,
            y: clamped_ry,
            w: clamped_rw,
            h: clamped_rh,
          }
          grid.fill_rect(rect, @types.Floor)
          let room : @types.Room = { rect, id: rooms.length() }
          rooms.push(room)
        }
      }
      // Check if reached target area
      let reach_dx = best_x - target_x
      let reach_dy = best_y - target_y
      if reach_dx * reach_dx + reach_dy * reach_dy <= 4 {
        // Mark target
        if grid.in_bounds(target_x, target_y) {
          grid.set(target_x, target_y, @types.Corridor)
          bolt_map[target_y * w + target_x] = true
        }
        // Remove this tip (reached goal)
        ignore(tips.remove(ti))
      }
      continue _s + 1
    }
    continue _b + 1
  }
  // Widen thick paths
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if thickness[y * w + x] > 2 {
        // Widen by making neighbors Floor
        let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) && grid.get(nx, ny) == @types.Wall {
            grid.set(nx, ny, @types.Floor)
          }
        }
        grid.set(x, y, @types.Floor)
      }
    }
  }
  { grid, rooms }
}
