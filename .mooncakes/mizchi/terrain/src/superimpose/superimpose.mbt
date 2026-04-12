///|
pub(all) struct SuperimposeConfig {
  width : Int
  height : Int
  num_stamps : Int
  rect_weight : Int
  circle_weight : Int
  corridor_h_weight : Int
  corridor_v_weight : Int
  stamp_min_size : Int
  stamp_max_size : Int
  corridor_min_len : Int
  corridor_max_len : Int
}

///|
pub fn SuperimposeConfig::default() -> SuperimposeConfig {
  {
    width: 50,
    height: 25,
    num_stamps: 40,
    rect_weight: 4,
    circle_weight: 2,
    corridor_h_weight: 3,
    corridor_v_weight: 3,
    stamp_min_size: 3,
    stamp_max_size: 8,
    corridor_min_len: 4,
    corridor_max_len: 12,
  }
}

///|
pub(all) struct SuperimposeResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : SuperimposeConfig,
  rng : @types.RNG
) -> SuperimposeResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let total_weight = config.rect_weight +
    config.circle_weight +
    config.corridor_h_weight +
    config.corridor_v_weight
  let mut room_id = 0
  for _ in 0..<config.num_stamps {
    let roll = rng.next_int(total_weight)
    if roll < config.rect_weight {
      // Rectangle stamp
      let w = rng.rand_range(config.stamp_min_size, config.stamp_max_size + 1)
      let h = rng.rand_range(config.stamp_min_size, config.stamp_max_size + 1)
      let x = rng.rand_range(1, config.width - w - 1)
      let y = rng.rand_range(1, config.height - h - 1)
      let rect : @types.Rect = { x, y, w, h }
      grid.fill_rect(rect, @types.Floor)
      rooms.push({ rect, id: room_id })
      room_id += 1
    } else if roll < config.rect_weight + config.circle_weight {
      // Circle stamp
      let radius = rng.rand_range(
        config.stamp_min_size / 2,
        config.stamp_max_size / 2 + 1,
      )
      let cx = rng.rand_range(radius + 1, config.width - radius - 1)
      let cy = rng.rand_range(radius + 1, config.height - radius - 1)
      carve_circle(grid, cx, cy, radius)
      rooms.push({
        rect: {
          x: (cx - radius).max(1),
          y: (cy - radius).max(1),
          w: radius * 2,
          h: radius * 2,
        },
        id: room_id,
      })
      room_id += 1
    } else if roll <
      config.rect_weight + config.circle_weight + config.corridor_h_weight {
      // Horizontal corridor
      let clen = rng.rand_range(config.corridor_min_len, config.corridor_max_len + 1)
      let x = rng.rand_range(1, (config.width - clen - 1).max(2))
      let y = rng.rand_range(1, config.height - 2)
      let rect : @types.Rect = { x, y, w: clen, h: 1 }
      grid.fill_rect(rect, @types.Floor)
    } else {
      // Vertical corridor
      let clen = rng.rand_range(config.corridor_min_len, config.corridor_max_len + 1)
      let x = rng.rand_range(1, config.width - 2)
      let y = rng.rand_range(1, (config.height - clen - 1).max(2))
      let rect : @types.Rect = { x, y, w: 1, h: clen }
      grid.fill_rect(rect, @types.Floor)
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, config)
  // Place Start and Goal
  let cx = config.width / 2
  let cy = config.height / 2
  let start = find_nearest_floor(grid, cx, cy)
  grid.set(start.x, start.y, @types.Start)
  let filled = @types.flood_fill(grid, start.x, start.y)
  let mut max_dist = 0
  let mut goal = start
  for p in filled {
    let dist = abs(p.x - start.x) + abs(p.y - start.y)
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
fn carve_circle(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int,
  radius : Int
) -> Unit {
  for dy in -radius..=radius {
    for dx in -radius..=radius {
      if dx * dx + dy * dy <= radius * radius {
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && x > 0 && y > 0 && x < grid.width - 1 &&
          y < grid.height - 1 {
          grid.set(x, y, @types.Floor)
        }
      }
    }
  }
}

///|
fn find_nearest_floor(grid : @types.Grid2D, cx : Int, cy : Int) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        if abs(dx) + abs(dy) != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) && grid.get(x, y) == @types.Floor {
          return { x, y }
        }
      }
    }
  }
  { x: cx, y: cy }
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : SuperimposeConfig
) -> Unit {
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      if not(visited[idx]) && grid.get(x, y) == @types.Floor {
        let comp = @types.flood_fill(grid, x, y)
        for p in comp {
          visited[p.y * config.width + p.x] = true
        }
        components.push(comp)
      }
    }
  }
  if components.length() <= 1 {
    return
  }
  let mut largest_idx = 0
  let mut largest_size = 0
  for i in 0..<components.length() {
    if components[i].length() > largest_size {
      largest_size = components[i].length()
      largest_idx = i
    }
  }
  for i in 0..<components.length() {
    if i == largest_idx {
      continue
    }
    let a = components[i][0]
    let b = components[largest_idx][0]
    grid.carve_l_corridor(a, b, @types.Floor)
  }
}
