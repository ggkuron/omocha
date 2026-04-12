///|
pub(all) struct PoissonConfig {
  width : Int
  height : Int
  min_distance : Int
  room_min_size : Int
  room_max_size : Int
  max_attempts : Int
}

///|
pub fn PoissonConfig::default() -> PoissonConfig {
  {
    width: 40,
    height: 20,
    min_distance: 8,
    room_min_size: 3,
    room_max_size: 6,
    max_attempts: 30,
  }
}

///|
pub(all) struct PoissonResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
fn distance(a : @types.Point, b : @types.Point) -> Double {
  let dx = (a.x - b.x).to_double()
  let dy = (a.y - b.y).to_double()
  (dx * dx + dy * dy).sqrt()
}

///|
fn poisson_disk_sample(
  width : Int,
  height : Int,
  min_dist : Int,
  max_attempts : Int,
  rng : @types.RNG
) -> Array[@types.Point] {
  let points : Array[@types.Point] = []
  let active : Array[Int] = []
  let first : @types.Point = {
    x: rng.rand_range(2, width - 2),
    y: rng.rand_range(2, height - 2),
  }
  points.push(first)
  active.push(0)
  while active.length() > 0 {
    let idx = rng.next_int(active.length())
    let point = points[active[idx]]
    let mut found = false
    for _ in 0..<max_attempts {
      // Generate random offset in range [min_dist, 2*min_dist)
      let dx = rng.rand_range(-min_dist * 2, min_dist * 2 + 1)
      let dy = rng.rand_range(-min_dist * 2, min_dist * 2 + 1)
      let dist_sq = (dx * dx + dy * dy).to_double()
      let min_sq = (min_dist * min_dist).to_double()
      let max_sq = (min_dist * min_dist * 4).to_double()
      if dist_sq < min_sq || dist_sq >= max_sq {
        continue
      }
      let nx = point.x + dx
      let ny = point.y + dy
      if nx < 2 || nx >= width - 2 || ny < 2 || ny >= height - 2 {
        continue
      }
      let candidate : @types.Point = { x: nx, y: ny }
      let mut too_close = false
      for p in points {
        if distance(candidate, p) < min_dist.to_double() {
          too_close = true
          break
        }
      }
      if not(too_close) {
        points.push(candidate)
        active.push(points.length() - 1)
        found = true
        break
      }
    }
    if not(found) {
      // Remove from active (swap with last)
      let last = active.length() - 1
      active[idx] = active[last]
      ignore(active.pop())
    }
  }
  points
}

///|
fn find_nearest_neighbors(
  points : Array[@types.Point],
  max_dist : Double
) -> Array[@types.Edge] {
  let edges : Array[@types.Edge] = []
  for i in 0..<points.length() {
    let mut best_dist = max_dist
    let mut best_j = -1
    for j in 0..<points.length() {
      if i == j {
        continue
      }
      let d = distance(points[i], points[j])
      if d < best_dist {
        best_dist = d
        best_j = j
      }
    }
    if best_j >= 0 {
      // Avoid duplicates
      let mut dup = false
      for e in edges {
        if (e.from == i && e.to == best_j) ||
          (e.from == best_j && e.to == i) {
          dup = true
          break
        }
      }
      if not(dup) {
        edges.push({ from: i, to: best_j })
      }
    }
  }
  // Add extra edges to ensure connectivity via MST-like approach
  let connected = Array::make(points.length(), false)
  if points.length() > 0 {
    connected[0] = true
    let mut components = points.length() - 1
    while components > 0 {
      let mut min_d = 99999.0
      let mut min_from = -1
      let mut min_to = -1
      for i in 0..<points.length() {
        if not(connected[i]) {
          continue
        }
        for j in 0..<points.length() {
          if connected[j] {
            continue
          }
          let d = distance(points[i], points[j])
          if d < min_d {
            min_d = d
            min_from = i
            min_to = j
          }
        }
      }
      if min_from >= 0 {
        connected[min_to] = true
        components -= 1
        let mut dup = false
        for e in edges {
          if (e.from == min_from && e.to == min_to) ||
            (e.from == min_to && e.to == min_from) {
            dup = true
            break
          }
        }
        if not(dup) {
          edges.push({ from: min_from, to: min_to })
        }
      } else {
        break
      }
    }
  }
  edges
}

///|
pub fn generate(config : PoissonConfig, rng : @types.RNG) -> PoissonResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let centers = poisson_disk_sample(
    config.width, config.height, config.min_distance, config.max_attempts, rng,
  )
  let rooms : Array[@types.Room] = []
  for i, center in centers {
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = (center.x - rw / 2).clamp(min=1, max=config.width - rw - 1)
    let ry = (center.y - rh / 2).clamp(min=1, max=config.height - rh - 1)
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: i })
  }
  // Connect rooms
  let edges = find_nearest_neighbors(
    centers, (config.width + config.height).to_double(),
  )
  for edge in edges {
    let from = centers[edge.from]
    let to = centers[edge.to]
    grid.carve_l_corridor(from, to, @types.Corridor)
  }
  { grid, rooms }
}
