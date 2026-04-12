///|
pub(all) struct SidewinderConfig {
  width : Int
  height : Int
  carve_chance : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn SidewinderConfig::default() -> SidewinderConfig {
  {
    width: 41,
    height: 21,
    carve_chance: 0.5,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct SidewinderResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : SidewinderConfig,
  rng : @types.RNG
) -> SidewinderResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  for row in 0..<maze_h {
    let gy = row * 2 + 1
    let mut run_start = 0
    for col in 0..<maze_w {
      let gx = col * 2 + 1
      grid.set(gx, gy, @types.Corridor)
      let at_east_boundary = col == maze_w - 1
      let at_north_boundary = row == 0
      let should_close = at_east_boundary ||
        (not(at_north_boundary) && rng.rand_bool(config.carve_chance))
      if should_close && not(at_north_boundary) {
        // Close run: pick random cell in run, carve north
        let pick = rng.rand_range(run_start, col + 1)
        let px = pick * 2 + 1
        grid.set(px, gy - 1, @types.Corridor) // wall above
        run_start = col + 1
      } else if not(at_east_boundary) {
        // Continue run: carve east
        grid.set(gx + 1, gy, @types.Corridor)
      }
    }
    // First row: carve all east walls
    if row == 0 {
      for col in 0..<(maze_w - 1) {
        let gx = col * 2 + 1
        grid.set(gx + 1, gy, @types.Corridor)
      }
    }
  }
  // Place rooms
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for _ in 0..<(maze_w * maze_h) {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(1, w - rw - 1)
    let ry = rng.rand_range(1, h - rh - 1)
    if rx + rw < w && ry + rh < h {
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for r in rooms {
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  // Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}
