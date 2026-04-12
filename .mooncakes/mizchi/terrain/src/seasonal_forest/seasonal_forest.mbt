///|
pub(all) struct SeasonalForestConfig {
  width : Int
  height : Int
  density_min : Double
  density_max : Double
  num_clearings : Int
}

///|
pub fn SeasonalForestConfig::default() -> SeasonalForestConfig {
  { width: 50, height: 30, density_min: 0.1, density_max: 0.7, num_clearings: 5 }
}

///|
pub(all) struct SeasonalForestResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : SeasonalForestConfig,
  rng : @types.RNG
) -> SeasonalForestResult {
  let w = config.width
  let h = config.height
  let grid = @types.Grid2D::make(w, h, @types.Floor)
  let rooms : Array[@types.Room] = []
  // Fill border with Wall
  for x in 0..<w {
    grid.set(x, 0, @types.Wall)
    grid.set(x, h - 1, @types.Wall)
  }
  for y in 0..<h {
    grid.set(0, y, @types.Wall)
    grid.set(w - 1, y, @types.Wall)
  }
  // Generate forest: density increases from bottom (y=h-1) to top (y=0)
  // y=0 is top (north, dense), y=h-1 is bottom (south, sparse)
  for y in 1..<(h - 1) {
    // Interpolate density: north (y=0) is dense, south (y=h-1) is sparse
    let t = y.to_double() / (h - 1).to_double()
    // t=0 at top (north), t=1 at bottom (south)
    // density should be max at top, min at bottom
    let density = config.density_max * (1.0 - t) + config.density_min * t
    for x in 1..<(w - 1) {
      if rng.rand_bool(density) {
        grid.set(x, y, @types.Wall)
      }
    }
  }
  // Carve clearings (rooms)
  let mut room_id = 0
  for _ in 0..<config.num_clearings {
    let rw = rng.rand_range(3, 7)
    let rh = rng.rand_range(3, 5)
    let rx = rng.rand_range(2, w - rw - 2)
    let ry = rng.rand_range(2, h - rh - 2)
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    // Clear the room area
    for cy in ry..<(ry + rh) {
      for cx in rx..<(rx + rw) {
        if grid.in_bounds(cx, cy) {
          grid.set(cx, cy, @types.Floor)
        }
      }
    }
    rooms.push({ rect, id: room_id })
    room_id += 1
  }
  // Create winding paths (Corridor) between clearings
  if rooms.length() > 1 {
    for i in 0..<(rooms.length() - 1) {
      let from = rooms[i].rect.center()
      let to = rooms[i + 1].rect.center()
      carve_winding_path(grid, from, to, rng, w, h)
    }
    // Connect last to first
    let from = rooms[rooms.length() - 1].rect.center()
    let to = rooms[0].rect.center()
    carve_winding_path(grid, from, to, rng, w, h)
  }
  // Add a few extra wandering paths through the forest
  for _ in 0..<3 {
    let sx = rng.rand_range(2, w - 2)
    let sy = h - 2 // Start from south
    let ex = rng.rand_range(2, w - 2)
    let ey = 1 // End at north
    carve_winding_path(
      grid,
      { x: sx, y: sy },
      { x: ex, y: ey },
      rng,
      w,
      h,
    )
  }
  // Place Start in south, Goal in north
  let start_pos = find_floor_near(grid, w / 2, h - 3)
  grid.set(start_pos.x, start_pos.y, @types.Start)
  let goal_pos = find_floor_near(grid, w / 2, 2)
  if goal_pos.x != start_pos.x || goal_pos.y != start_pos.y {
    grid.set(goal_pos.x, goal_pos.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn carve_winding_path(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  rng : @types.RNG,
  w : Int,
  h : Int
) -> Unit {
  let mut x = from.x
  let mut y = from.y
  while x != to.x || y != to.y {
    if x >= 1 && x < w - 1 && y >= 1 && y < h - 1 {
      let current = grid.get(x, y)
      if current == @types.Wall {
        grid.set(x, y, @types.Corridor)
      }
    }
    // Move towards target with random wobble
    if rng.rand_bool(0.3) {
      // Random step
      let dir = rng.rand_direction()
      let nx = x + dir.x
      let ny = y + dir.y
      if nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 {
        x = nx
        y = ny
      }
    } else {
      // Move toward target
      if x != to.x && (y == to.y || rng.rand_bool(0.5)) {
        if x < to.x {
          x += 1
        } else {
          x -= 1
        }
      } else if y != to.y {
        if y < to.y {
          y += 1
        } else {
          y -= 1
        }
      }
    }
  }
  if grid.in_bounds(to.x, to.y) && grid.get(to.x, to.y) == @types.Wall {
    grid.set(to.x, to.y, @types.Corridor)
  }
}

///|
fn find_floor_near(
  grid : @types.Grid2D,
  cx : Int,
  cy : Int
) -> @types.Point {
  for radius in 0..<(grid.width + grid.height) {
    for dy in -radius..=radius {
      for dx in -radius..=radius {
        let adx = if dx < 0 { -dx } else { dx }
        let ady = if dy < 0 { -dy } else { dy }
        if adx + ady != radius {
          continue
        }
        let x = cx + dx
        let y = cy + dy
        if grid.in_bounds(x, y) {
          let t = grid.get(x, y)
          if t == @types.Floor || t == @types.Corridor {
            return { x, y }
          }
        }
      }
    }
  }
  { x: cx, y: cy }
}
