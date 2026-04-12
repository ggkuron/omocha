///|
pub(all) struct HerringboneConfig {
  width : Int
  height : Int
  tile_size : Int
  floor_ratio : Double
  corridor_chance : Double
}

///|
pub fn HerringboneConfig::default() -> HerringboneConfig {
  { width: 50, height: 25, tile_size: 6, floor_ratio: 0.5, corridor_chance: 0.3 }
}

///|
pub(all) struct HerringboneResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

// Tile template types
// 0 = wall block (all walls)
// 1 = empty room (all floor)
// 2 = corridor NS (vertical corridor through center)
// 3 = corridor EW (horizontal corridor through center)
// 4 = L-turn (floor in L shape)
// 5 = T-junction (floor in T shape)
// 6 = crossroads (floor in + shape)

///|
fn stamp_template(
  grid : @types.Grid2D,
  template_id : Int,
  ox : Int,
  oy : Int,
  tw : Int,
  th : Int
) -> @types.Rect? {
  match template_id {
    0 => {
      // Wall block: leave all as Wall
      None
    }
    1 => {
      // Empty room: fill interior with Floor, leave 1-cell wall border
      let inner_rect : @types.Rect = {
        x: ox + 1,
        y: oy + 1,
        w: if tw > 2 { tw - 2 } else { tw },
        h: if th > 2 { th - 2 } else { th },
      }
      for y in inner_rect.y..<(inner_rect.y + inner_rect.h) {
        for x in inner_rect.x..<(inner_rect.x + inner_rect.w) {
          if grid.in_bounds(x, y) {
            grid.set(x, y, @types.Floor)
          }
        }
      }
      Some(inner_rect)
    }
    2 => {
      // Corridor NS: vertical corridor through center
      let cx = ox + tw / 2
      for y in oy..<(oy + th) {
        if grid.in_bounds(cx, y) {
          grid.set(cx, y, @types.Corridor)
        }
      }
      None
    }
    3 => {
      // Corridor EW: horizontal corridor through center
      let cy = oy + th / 2
      for x in ox..<(ox + tw) {
        if grid.in_bounds(x, cy) {
          grid.set(x, cy, @types.Corridor)
        }
      }
      None
    }
    4 => {
      // L-turn: floor in L shape (bottom-left corner)
      let cy = oy + th / 2
      let cx = ox + tw / 2
      // Horizontal segment from left to center
      for x in ox..<(cx + 1) {
        if grid.in_bounds(x, cy) {
          grid.set(x, cy, @types.Corridor)
        }
      }
      // Vertical segment from center down
      for y in cy..<(oy + th) {
        if grid.in_bounds(cx, y) {
          grid.set(cx, y, @types.Corridor)
        }
      }
      None
    }
    5 => {
      // T-junction: horizontal corridor with branch down from center
      let cy = oy + th / 2
      let cx = ox + tw / 2
      for x in ox..<(ox + tw) {
        if grid.in_bounds(x, cy) {
          grid.set(x, cy, @types.Corridor)
        }
      }
      for y in cy..<(oy + th) {
        if grid.in_bounds(cx, y) {
          grid.set(cx, y, @types.Corridor)
        }
      }
      None
    }
    6 => {
      // Crossroads: + shape through center
      let cy = oy + th / 2
      let cx = ox + tw / 2
      for x in ox..<(ox + tw) {
        if grid.in_bounds(x, cy) {
          grid.set(x, cy, @types.Corridor)
        }
      }
      for y in oy..<(oy + th) {
        if grid.in_bounds(cx, y) {
          grid.set(cx, y, @types.Corridor)
        }
      }
      None
    }
    _ => None
  }
}

///|
fn choose_template(rng : @types.RNG, floor_ratio : Double) -> Int {
  // Higher floor_ratio = more open tiles (rooms, crossroads, T-junctions)
  // Lower floor_ratio = more walls and simple corridors
  if rng.rand_bool(floor_ratio) {
    // Open tiles: room, T-junction, crossroads
    let open_templates : Array[Int] = [1, 5, 6]
    rng.rand_pick(open_templates)
  } else {
    // Closed tiles: wall, corridor NS, corridor EW, L-turn
    let closed_templates : Array[Int] = [0, 2, 3, 4]
    rng.rand_pick(closed_templates)
  }
}

///|
pub fn generate(
  config : HerringboneConfig,
  rng : @types.RNG
) -> HerringboneResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  let ts = config.tile_size
  // Calculate grid dimensions in tile units
  let cols = (config.width + ts - 1) / ts
  let rows = (config.height + ts - 1) / ts
  // Place tiles in herringbone pattern
  // Herringbone: alternating horizontal and vertical rectangles
  for gy in 0..<rows {
    for gx in 0..<cols {
      let template_id = choose_template(rng, config.floor_ratio)
      if (gx + gy) % 2 == 0 {
        // Horizontal tile: width=ts, height=ts/2 (at least 2)
        let tw = ts
        let th = if ts / 2 > 2 { ts / 2 } else { 2 }
        let ox = gx * ts
        let oy = gy * ts
        if ox + tw <= config.width && oy + th <= config.height {
          let maybe_rect = stamp_template(grid, template_id, ox, oy, tw, th)
          match maybe_rect {
            Some(r) => {
              rooms.push({ rect: r, id: room_id })
              room_id += 1
            }
            None => ()
          }
        }
        // Place a second horizontal tile below to fill the gap
        let oy2 = gy * ts + th
        if ox + tw <= config.width && oy2 + th <= config.height {
          let template_id2 = choose_template(rng, config.floor_ratio)
          let maybe_rect2 = stamp_template(
            grid, template_id2, ox, oy2, tw, th,
          )
          match maybe_rect2 {
            Some(r) => {
              rooms.push({ rect: r, id: room_id })
              room_id += 1
            }
            None => ()
          }
        }
      } else {
        // Vertical tile: width=ts/2 (at least 2), height=ts
        let tw = if ts / 2 > 2 { ts / 2 } else { 2 }
        let th = ts
        let ox = gx * ts
        let oy = gy * ts
        if ox + tw <= config.width && oy + th <= config.height {
          let maybe_rect = stamp_template(grid, template_id, ox, oy, tw, th)
          match maybe_rect {
            Some(r) => {
              rooms.push({ rect: r, id: room_id })
              room_id += 1
            }
            None => ()
          }
        }
        // Place a second vertical tile to the right
        let ox2 = gx * ts + tw
        if ox2 + tw <= config.width && oy + th <= config.height {
          let template_id2 = choose_template(rng, config.floor_ratio)
          let maybe_rect2 = stamp_template(
            grid, template_id2, ox2, oy, tw, th,
          )
          match maybe_rect2 {
            Some(r) => {
              rooms.push({ rect: r, id: room_id })
              room_id += 1
            }
            None => ()
          }
        }
      }
    }
  }
  // Add corridors between adjacent tiles with corridor_chance
  // Scan for corridor connections: find floor/corridor cells adjacent to walls
  // that separate two open areas
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if grid.get(x, y) != @types.Wall {
        continue
      }
      if not(rng.rand_bool(config.corridor_chance)) {
        continue
      }
      // Check if this wall separates two open areas (horizontal)
      let left_open = grid.in_bounds(x - 1, y) &&
        (grid.get(x - 1, y) == @types.Floor ||
        grid.get(x - 1, y) == @types.Corridor)
      let right_open = grid.in_bounds(x + 1, y) &&
        (grid.get(x + 1, y) == @types.Floor ||
        grid.get(x + 1, y) == @types.Corridor)
      if left_open && right_open {
        grid.set(x, y, @types.Corridor)
        continue
      }
      // Check vertical separation
      let up_open = grid.in_bounds(x, y - 1) &&
        (grid.get(x, y - 1) == @types.Floor ||
        grid.get(x, y - 1) == @types.Corridor)
      let down_open = grid.in_bounds(x, y + 1) &&
        (grid.get(x, y + 1) == @types.Floor ||
        grid.get(x, y + 1) == @types.Corridor)
      if up_open && down_open {
        grid.set(x, y, @types.Corridor)
      }
    }
  }
  { grid, rooms }
}
