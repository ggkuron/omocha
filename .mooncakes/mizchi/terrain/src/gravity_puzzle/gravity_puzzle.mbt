///|
pub(all) struct GravityPuzzleConfig {
  width : Int
  height : Int
  num_platforms : Int
  inversion_y : Double
}

///|
pub fn GravityPuzzleConfig::default() -> GravityPuzzleConfig {
  { width: 50, height: 30, num_platforms: 10, inversion_y: 0.5 }
}

///|
pub(all) struct GravityPuzzleResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Place a normal-gravity platform (floor on wall base):
/// Wall row at bottom, Floor row(s) on top
fn place_normal_platform(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  pw : Int,
  ph : Int
) -> Unit {
  // Base wall row at bottom
  for dx in 0..<pw {
    let px = x + dx
    if grid.in_bounds(px, y + ph - 1) {
      grid.set(px, y + ph - 1, @types.Wall)
    }
  }
  // Floor rows on top of the wall base
  for dy in 0..<(ph - 1) {
    for dx in 0..<pw {
      let px = x + dx
      let py = y + dy
      if grid.in_bounds(px, py) {
        grid.set(px, py, @types.Floor)
      }
    }
  }
}

///| Place an inverted-gravity platform (floor on ceiling):
/// Wall row at top, Floor row(s) hanging below
fn place_inverted_platform(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  pw : Int,
  ph : Int
) -> Unit {
  // Ceiling wall row at top
  for dx in 0..<pw {
    let px = x + dx
    if grid.in_bounds(px, y) {
      grid.set(px, y, @types.Wall)
    }
  }
  // Floor rows below the ceiling
  for dy in 1..<ph {
    for dx in 0..<pw {
      let px = x + dx
      let py = y + dy
      if grid.in_bounds(px, py) {
        grid.set(px, py, @types.Floor)
      }
    }
  }
}

///| Place a vertical ladder (Corridor tiles) connecting two y positions
fn place_ladder(
  grid : @types.Grid2D,
  x : Int,
  y_start : Int,
  y_end : Int
) -> Unit {
  let y_min = if y_start < y_end { y_start } else { y_end }
  let y_max = if y_start > y_end { y_start } else { y_end }
  for y in y_min..=y_max {
    if grid.in_bounds(x, y) {
      grid.set(x, y, @types.Corridor)
    }
  }
}

///| Place water at the boundary between normal and inverted zones
fn place_water_boundary(
  grid : @types.Grid2D,
  width : Int,
  boundary_y : Int
) -> Unit {
  for x in 0..<width {
    if grid.in_bounds(x, boundary_y) && grid.get(x, boundary_y) == @types.Empty {
      grid.set(x, boundary_y, @types.Water)
    }
    // Also one row above and below for thickness
    if grid.in_bounds(x, boundary_y - 1) &&
      grid.get(x, boundary_y - 1) == @types.Empty {
      grid.set(x, boundary_y - 1, @types.Water)
    }
    if grid.in_bounds(x, boundary_y + 1) &&
      grid.get(x, boundary_y + 1) == @types.Empty {
      grid.set(x, boundary_y + 1, @types.Water)
    }
  }
}

///|
pub fn generate(
  config : GravityPuzzleConfig,
  rng : @types.RNG
) -> GravityPuzzleResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Empty)
  let rooms : Array[@types.Room] = []
  // Boundary between normal (upper) and inverted (lower) zones
  let boundary_y = (config.height.to_double() * config.inversion_y).to_int()
  // Distribute platforms between upper (normal gravity) and lower (inverted gravity)
  let upper_count = config.num_platforms / 2
  let lower_count = config.num_platforms - upper_count
  // Place normal gravity platforms in upper half
  let min_pw = 4
  let max_pw = 8
  let platform_h = 3
  for i in 0..<upper_count {
    ignore(i)
    let pw = rng.rand_range(min_pw, max_pw + 1)
    let mut placed = false
    for _ in 0..<40 {
      let px = rng.rand_range(1, config.width - pw - 1)
      // Place in upper half, with some vertical spread
      let py = rng.rand_range(2, boundary_y - platform_h - 1)
      if py < 1 {
        continue
      }
      // Check for overlap with existing rooms
      let rect : @types.Rect = { x: px, y: py, w: pw, h: platform_h }
      let mut overlaps = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        place_normal_platform(grid, px, py, pw, platform_h)
        rooms.push({ rect, id: rooms.length() })
        placed = true
        break
      }
    }
    if not(placed) {
      let px = rng.rand_range(1, config.width - pw - 1)
      let py = rng.rand_range(2, boundary_y - platform_h - 1)
      let safe_py = if py < 1 { 2 } else { py }
      let rect : @types.Rect = { x: px, y: safe_py, w: pw, h: platform_h }
      place_normal_platform(grid, px, safe_py, pw, platform_h)
      rooms.push({ rect, id: rooms.length() })
    }
  }
  // Place inverted gravity platforms in lower half
  for i in 0..<lower_count {
    ignore(i)
    let pw = rng.rand_range(min_pw, max_pw + 1)
    let mut placed = false
    for _ in 0..<40 {
      let px = rng.rand_range(1, config.width - pw - 1)
      let py = rng.rand_range(boundary_y + 2, config.height - platform_h - 1)
      if py >= config.height - platform_h {
        continue
      }
      let rect : @types.Rect = { x: px, y: py, w: pw, h: platform_h }
      let mut overlaps = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        place_inverted_platform(grid, px, py, pw, platform_h)
        rooms.push({ rect, id: rooms.length() })
        placed = true
        break
      }
    }
    if not(placed) {
      let px = rng.rand_range(1, config.width - pw - 1)
      let py = rng.rand_range(boundary_y + 2, config.height - platform_h - 1)
      let safe_py = if py >= config.height - platform_h {
        config.height - platform_h - 1
      } else {
        py
      }
      let rect : @types.Rect = { x: px, y: safe_py, w: pw, h: platform_h }
      place_inverted_platform(grid, px, safe_py, pw, platform_h)
      rooms.push({ rect, id: rooms.length() })
    }
  }
  // Place ladders connecting some platforms vertically
  // Sort rooms by x position and connect nearby platforms with ladders
  for i in 0..<rooms.length() {
    for j in (i + 1)..<rooms.length() {
      let ri = rooms[i]
      let rj = rooms[j]
      // Check if platforms overlap horizontally
      let overlap_x_start = if ri.rect.x > rj.rect.x {
        ri.rect.x
      } else {
        rj.rect.x
      }
      let ri_end = ri.rect.x + ri.rect.w
      let rj_end = rj.rect.x + rj.rect.w
      let overlap_x_end = if ri_end < rj_end { ri_end } else { rj_end }
      if overlap_x_start < overlap_x_end {
        // Pick a random x within the overlap for the ladder
        let ladder_x = rng.rand_range(overlap_x_start, overlap_x_end)
        let y1 = ri.rect.y + ri.rect.h - 1
        let y2 = rj.rect.y
        if y1 < y2 {
          place_ladder(grid, ladder_x, y1, y2)
        } else {
          let y1_alt = rj.rect.y + rj.rect.h - 1
          let y2_alt = ri.rect.y
          if y1_alt < y2_alt {
            place_ladder(grid, ladder_x, y1_alt, y2_alt)
          }
        }
      }
    }
  }
  // Place water at the boundary
  place_water_boundary(grid, config.width, boundary_y)
  // Place Start on first upper platform, Goal on last lower platform
  if rooms.length() > 0 {
    let start_room = rooms[0]
    let start_pos = start_room.rect.center()
    if grid.in_bounds(start_pos.x, start_pos.y) {
      grid.set(start_pos.x, start_pos.y, @types.Start)
    }
  }
  if rooms.length() > 1 {
    let goal_room = rooms[rooms.length() - 1]
    let goal_pos = goal_room.rect.center()
    if grid.in_bounds(goal_pos.x, goal_pos.y) {
      grid.set(goal_pos.x, goal_pos.y, @types.Goal)
    }
  }
  { grid, rooms }
}
