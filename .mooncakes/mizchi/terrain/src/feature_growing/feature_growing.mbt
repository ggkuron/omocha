///|
pub(all) struct FeatureGrowingConfig {
  width : Int
  height : Int
  max_features : Int
  max_attempts : Int
  room_min_size : Int
  room_max_size : Int
  corridor_min_len : Int
  corridor_max_len : Int
  room_weight : Int
  corridor_weight : Int
}

///|
pub fn FeatureGrowingConfig::default() -> FeatureGrowingConfig {
  {
    width: 50,
    height: 25,
    max_features: 30,
    max_attempts: 500,
    room_min_size: 3,
    room_max_size: 7,
    corridor_min_len: 3,
    corridor_max_len: 8,
    room_weight: 3,
    corridor_weight: 5,
  }
}

///|
pub(all) struct FeatureGrowingResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : FeatureGrowingConfig,
  rng : @types.RNG
) -> FeatureGrowingResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Place first room at center
  let fw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
  let fh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
  let first_rect : @types.Rect = {
    x: config.width / 2 - fw / 2,
    y: config.height / 2 - fh / 2,
    w: fw,
    h: fh,
  }
  grid.fill_rect(first_rect, @types.Floor)
  rooms.push({ rect: first_rect, id: 0 })
  let mut features_placed = 1
  let total_weight = config.room_weight + config.corridor_weight
  for _ in 0..<config.max_attempts {
    if features_placed >= config.max_features {
      break
    }
    // Find a random wall cell adjacent to floor
    let attachment = find_wall_adjacent_floor(grid, config, rng)
    match attachment {
      None => continue
      Some((wx, wy, dx, dy)) => {
        // Choose feature type by weight
        let roll = rng.next_int(total_weight)
        if roll < config.room_weight {
          // Try to place a room
          let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
          let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
          let rect = compute_feature_rect(wx, wy, dx, dy, rw, rh, rng)
          if can_place(grid, rect, config) {
            grid.fill_rect(rect, @types.Floor)
            grid.set(wx, wy, @types.Door)
            rooms.push({ rect, id: features_placed })
            features_placed += 1
          }
        } else {
          // Try to place a corridor
          let clen = rng.rand_range(
            config.corridor_min_len,
            config.corridor_max_len + 1,
          )
          let rect = if dx != 0 {
            let cx = if dx > 0 { wx + 1 } else { wx - clen }
            @types.Rect::{ x: cx, y: wy, w: clen, h: 1 }
          } else {
            let cy = if dy > 0 { wy + 1 } else { wy - clen }
            @types.Rect::{ x: wx, y: cy, w: 1, h: clen }
          }
          if can_place(grid, rect, config) {
            grid.fill_rect(rect, @types.Corridor)
            grid.set(wx, wy, @types.Door)
            features_placed += 1
          }
        }
      }
    }
  }
  // Place Start and Goal
  if rooms.length() > 0 {
    let sc = rooms[0].rect.center()
    grid.set(sc.x, sc.y, @types.Start)
  }
  if rooms.length() > 1 {
    let gc = rooms[rooms.length() - 1].rect.center()
    grid.set(gc.x, gc.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn find_wall_adjacent_floor(
  grid : @types.Grid2D,
  config : FeatureGrowingConfig,
  rng : @types.RNG
) -> (Int, Int, Int, Int)? {
  let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  for _ in 0..<80 {
    let x = rng.rand_range(2, config.width - 2)
    let y = rng.rand_range(2, config.height - 2)
    if grid.get(x, y) != @types.Wall {
      continue
    }
    for d in dirs {
      let nx = x + d.0
      let ny = y + d.1
      if grid.in_bounds(nx, ny) {
        let t = grid.get(nx, ny)
        if t == @types.Floor || t == @types.Corridor || t == @types.Start {
          return Some((x, y, -d.0, -d.1))
        }
      }
    }
  }
  None
}

///|
fn compute_feature_rect(
  wx : Int,
  wy : Int,
  dx : Int,
  dy : Int,
  w : Int,
  h : Int,
  rng : @types.RNG
) -> @types.Rect {
  if dx > 0 {
    { x: wx + 1, y: wy - rng.next_int(h), w, h }
  } else if dx < 0 {
    { x: wx - w, y: wy - rng.next_int(h), w, h }
  } else if dy > 0 {
    { x: wx - rng.next_int(w), y: wy + 1, w, h }
  } else {
    { x: wx - rng.next_int(w), y: wy - h, w, h }
  }
}

///|
fn can_place(
  grid : @types.Grid2D,
  rect : @types.Rect,
  config : FeatureGrowingConfig
) -> Bool {
  // Check bounds with padding
  if rect.x < 1 ||
    rect.y < 1 ||
    rect.x + rect.w >= config.width - 1 ||
    rect.y + rect.h >= config.height - 1 {
    return false
  }
  // Check all cells (with 1-cell padding) are wall
  for y in (rect.y - 1)..<(rect.y + rect.h + 1) {
    for x in (rect.x - 1)..<(rect.x + rect.w + 1) {
      if not(grid.in_bounds(x, y)) {
        return false
      }
      if grid.get(x, y) != @types.Wall {
        return false
      }
    }
  }
  true
}
