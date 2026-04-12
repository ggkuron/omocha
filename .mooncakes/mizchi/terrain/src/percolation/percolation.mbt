///|
pub(all) struct PercolationConfig {
  width : Int
  height : Int
  open_chance : Double
  widen : Int
  min_component_size : Int
}

///|
pub fn PercolationConfig::default() -> PercolationConfig {
  { width: 50, height: 25, open_chance: 0.55, widen: 1, min_component_size: 10 }
}

///|
pub(all) struct PercolationResult {
  grid : @types.Grid2D
  open_cells : Int
}

///|
pub fn generate(
  config : PercolationConfig,
  rng : @types.RNG
) -> PercolationResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Step 1: Randomly open cells
  for y in 0..<h {
    for x in 0..<w {
      if rng.rand_bool(config.open_chance) {
        grid.set(x, y, @types.Floor)
      }
    }
  }
  // Step 2: Label connected components using flood fill
  let labels : Array[Int] = Array::make(w * h, -1)
  let mut next_label = 0
  let component_sizes : Array[Int] = []
  for y in 0..<h {
    for x in 0..<w {
      if grid.get(x, y) == @types.Floor && labels[y * w + x] == -1 {
        // BFS flood fill
        let label = next_label
        next_label = next_label + 1
        component_sizes.push(0)
        let queue : Array[(Int, Int)] = [(x, y)]
        labels[y * w + x] = label
        let mut head = 0
        while head < queue.length() {
          let pos = queue[head]
          head = head + 1
          let px = pos.0
          let py = pos.1
          component_sizes[label] = component_sizes[label] + 1
          let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
          for d in dirs {
            let nx = px + d.0
            let ny = py + d.1
            if nx >= 0 &&
              nx < w &&
              ny >= 0 &&
              ny < h &&
              grid.get(nx, ny) == @types.Floor &&
              labels[ny * w + nx] == -1 {
              labels[ny * w + nx] = label
              queue.push((nx, ny))
            }
          }
        }
      }
    }
  }
  // Step 3: Find spanning clusters (touching both top and bottom edges)
  let touches_top : Array[Bool] = Array::make(next_label, false)
  let touches_bottom : Array[Bool] = Array::make(next_label, false)
  for x in 0..<w {
    let top_label = labels[0 * w + x]
    if top_label >= 0 {
      touches_top[top_label] = true
    }
    let bottom_label = labels[(h - 1) * w + x]
    if bottom_label >= 0 {
      touches_bottom[bottom_label] = true
    }
  }
  // Find the largest spanning cluster
  let mut best_label = -1
  let mut best_size = 0
  for i in 0..<next_label {
    if touches_top[i] && touches_bottom[i] && component_sizes[i] >= config.min_component_size {
      if component_sizes[i] > best_size {
        best_size = component_sizes[i]
        best_label = i
      }
    }
  }
  // If no spanning cluster found, pick the largest component overall
  if best_label == -1 {
    for i in 0..<next_label {
      if component_sizes[i] > best_size {
        best_size = component_sizes[i]
        best_label = i
      }
    }
  }
  // Step 4: Keep only the selected component
  for y in 0..<h {
    for x in 0..<w {
      if labels[y * w + x] != best_label {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Step 5: Widen corridors
  if config.widen > 0 {
    // Clone grid to read from original while writing
    let original = grid.clone()
    for y in 0..<h {
      for x in 0..<w {
        if original.get(x, y) == @types.Floor {
          for dy in -(config.widen)..=(config.widen) {
            for dx in -(config.widen)..=(config.widen) {
              let nx = x + dx
              let ny = y + dy
              if grid.in_bounds(nx, ny) {
                grid.set(nx, ny, @types.Floor)
              }
            }
          }
        }
      }
    }
  }
  // Count open cells
  let mut open_cells = 0
  for y in 0..<h {
    for x in 0..<w {
      if grid.get(x, y) == @types.Floor {
        open_cells = open_cells + 1
      }
    }
  }
  { grid, open_cells }
}
