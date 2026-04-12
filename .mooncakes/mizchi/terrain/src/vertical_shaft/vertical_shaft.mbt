///|
pub(all) struct VerticalShaftConfig {
  width : Int
  height : Int
  depth : Int
  num_shafts : Int
  room_chance : Double
}

///|
pub fn VerticalShaftConfig::default() -> VerticalShaftConfig {
  { width: 40, height: 20, depth: 4, num_shafts: 3, room_chance: 0.6 }
}

///|
pub(all) struct VerticalShaftResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : VerticalShaftConfig,
  rng : @types.RNG
) -> VerticalShaftResult {
  let grid = @types.Grid3D::make(
    config.width, config.height, config.depth, @types.Wall,
  )
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Generate rooms on each layer
  for z in 0..<config.depth {
    let layer = grid.get_layer(z)
    let max_room_attempts = 15
    for _attempt in 0..<max_room_attempts {
      if not(rng.rand_bool(config.room_chance)) {
        continue
      }
      let rw = rng.rand_range(4, 10)
      let rh = rng.rand_range(4, 8)
      let rx = rng.rand_range(1, config.width - rw - 1)
      let ry = rng.rand_range(1, config.height - rh - 1)
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      // Check no overlap with existing rooms on this layer
      let mut overlaps = false
      for r in rooms {
        // Only check rooms we already placed; approximate by just checking
        // the grid for existing floor tiles
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          // Check if this room is on the same layer by testing a cell
          if layer.in_bounds(r.rect.x, r.rect.y) &&
            layer.get(r.rect.x, r.rect.y) == @types.Floor {
            overlaps = true
            break
          }
        }
      }
      if not(overlaps) {
        layer.fill_rect(rect, @types.Floor)
        let room : @types.Room = { rect, id: room_id }
        rooms.push(room)
        room_id += 1
      }
    }
    // Connect rooms on this layer with corridors
    let layer_rooms : Array[@types.Room] = []
    for r in rooms {
      if layer.in_bounds(r.rect.x, r.rect.y) &&
        layer.get(r.rect.x, r.rect.y) == @types.Floor {
        layer_rooms.push(r)
      }
    }
    for i in 1..<layer_rooms.length() {
      let c1 = layer_rooms[i - 1].rect.center()
      let c2 = layer_rooms[i].rect.center()
      layer.carve_l_corridor(c1, c2, @types.Corridor)
    }
  }
  // Punch vertical shafts through all layers
  for _s in 0..<config.num_shafts {
    // Pick shaft size: 2x2 or 3x3
    let shaft_size = if rng.rand_bool(0.5) { 2 } else { 3 }
    let sx = rng.rand_range(2, config.width - shaft_size - 2)
    let sy = rng.rand_range(2, config.height - shaft_size - 2)
    // Carve the shaft through all layers
    for z in 0..<config.depth {
      for dy in 0..<shaft_size {
        for dx in 0..<shaft_size {
          grid.set(sx + dx, sy + dy, z, @types.Floor)
        }
      }
    }
    // Connect shaft to nearest room on each layer via corridor
    for z in 0..<config.depth {
      let layer = grid.get_layer(z)
      let shaft_center : @types.Point = {
        x: sx + shaft_size / 2,
        y: sy + shaft_size / 2,
      }
      // Find closest room center on this layer
      let mut best_dist = 999999
      let mut best_center : @types.Point = shaft_center
      for r in rooms {
        if layer.in_bounds(r.rect.x, r.rect.y) &&
          layer.get(r.rect.x, r.rect.y) == @types.Floor {
          let rc = r.rect.center()
          let dx = rc.x - shaft_center.x
          let dy = rc.y - shaft_center.y
          let dist = dx * dx + dy * dy
          if dist < best_dist {
            best_dist = dist
            best_center = rc
          }
        }
      }
      if best_dist < 999999 {
        layer.carve_l_corridor(shaft_center, best_center, @types.Corridor)
      }
    }
  }
  { grid, rooms }
}
