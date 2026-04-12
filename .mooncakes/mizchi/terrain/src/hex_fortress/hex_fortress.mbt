///|
pub(all) struct HexFortressConfig {
  cols : Int
  rows : Int
  num_walls : Int
  gate_count : Int
}

///|
pub fn HexFortressConfig::default() -> HexFortressConfig {
  { cols: 25, rows: 20, num_walls: 3, gate_count: 2 }
}

///|
pub(all) struct HexFortressResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn hex_dist(c1 : Int, r1 : Int, c2 : Int, r2 : Int) -> Int {
  // Convert odd-r offset to cube coordinates
  let x1 = c1 - (r1 - (r1 & 1)) / 2
  let z1 = r1
  let y1 = -x1 - z1
  let x2 = c2 - (r2 - (r2 & 1)) / 2
  let z2 = r2
  let y2 = -x2 - z2
  let dx = if x1 > x2 { x1 - x2 } else { x2 - x1 }
  let dy = if y1 > y2 { y1 - y2 } else { y2 - y1 }
  let dz = if z1 > z2 { z1 - z2 } else { z2 - z1 }
  let mut m = dx
  if dy > m {
    m = dy
  }
  if dz > m {
    m = dz
  }
  m
}

///|
pub fn generate(
  config : HexFortressConfig,
  rng : @types.RNG
) -> HexFortressResult {
  let cols = config.cols
  let rows = config.rows
  let grid = @types.HexGrid::make(cols, rows, @types.Floor)
  let rooms : Array[@types.Room] = []
  let cx = cols / 2
  let cy = rows / 2
  // Calculate wall ring radii
  let max_radius = if cols < rows { cols / 2 - 1 } else { rows / 2 - 1 }
  let wall_radii : Array[Int] = []
  for i in 0..<config.num_walls {
    let radius = max_radius * (i + 1) / config.num_walls
    if radius > 0 {
      wall_radii.push(radius)
    }
  }
  // Draw concentric hexagonal walls
  for r in 0..<rows {
    for c in 0..<cols {
      let dist = hex_dist(c, r, cx, cy)
      for wr in wall_radii {
        if dist == wr {
          grid.set(c, r, @types.Wall)
        }
      }
    }
  }
  // Place gates (Doors) on each wall ring
  for wi in 0..<wall_radii.length() {
    let wr = wall_radii[wi]
    // Collect all wall cells on this ring
    let ring_cells : Array[(Int, Int)] = []
    for r in 0..<rows {
      for c in 0..<cols {
        if hex_dist(c, r, cx, cy) == wr && grid.get(c, r) == @types.Wall {
          ring_cells.push((c, r))
        }
      }
    }
    if ring_cells.length() > 0 {
      // Place gates evenly spaced
      let gate_count = if config.gate_count > ring_cells.length() {
        ring_cells.length()
      } else {
        config.gate_count
      }
      let spacing = ring_cells.length() / gate_count
      for gi in 0..<gate_count {
        let offset = rng.next_int(spacing / 2 + 1)
        let idx = (gi * spacing + offset) % ring_cells.length()
        let cell = ring_cells[idx]
        grid.set(cell.0, cell.1, @types.Door)
      }
    }
  }
  // Place tower clusters (3x3-ish Wall formations) at hexagonal vertices
  // Vertices are at the 6 directions from center at wall radius
  let vertex_dirs : Array[(Int, Int)] = [
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
    (1, -1),
    (-1, 1),
  ]
  for wr in wall_radii {
    for dir in vertex_dirs {
      let tc = cx + dir.0 * wr
      let tr = cy + dir.1 * wr
      // Place tower: thick wall cluster around vertex
      for dr in -1..=1 {
        for dc in -1..=1 {
          let nc = tc + dc
          let nr = tr + dr
          if grid.in_bounds(nc, nr) {
            let dist = hex_dist(nc, nr, cx, cy)
            // Only place if near the wall ring
            let diff = if dist > wr { dist - wr } else { wr - dist }
            if diff <= 1 {
              grid.set(nc, nr, @types.Wall)
            }
          }
        }
      }
    }
  }
  // Mark the keep (center area) with Start
  grid.set(cx, cy, @types.Start)
  // Build rooms for each ring zone (courtyard between walls)
  let mut room_id = 0
  // Inner keep
  let keep_rect : @types.Rect = {
    x: cx - 1,
    y: cy - 1,
    w: 3,
    h: 3,
  }
  rooms.push({ rect: keep_rect, id: room_id })
  room_id += 1
  // Courtyards between walls
  for wi in 0..<wall_radii.length() {
    let inner_r = if wi == 0 { 0 } else { wall_radii[wi - 1] }
    let outer_r = wall_radii[wi]
    // Find bounding box of courtyard cells
    let mut min_c = cols
    let mut max_c = 0
    let mut min_r = rows
    let mut max_r = 0
    let mut has_cells = false
    for r in 0..<rows {
      for c in 0..<cols {
        let dist = hex_dist(c, r, cx, cy)
        if dist > inner_r && dist < outer_r && grid.get(c, r) == @types.Floor {
          has_cells = true
          if c < min_c {
            min_c = c
          }
          if c > max_c {
            max_c = c
          }
          if r < min_r {
            min_r = r
          }
          if r > max_r {
            max_r = r
          }
        }
      }
    }
    if has_cells {
      rooms.push(
        {
          rect: { x: min_c, y: min_r, w: max_c - min_c + 1, h: max_r - min_r + 1 },
          id: room_id,
        },
      )
      room_id += 1
    }
  }
  // Outer area room
  let outer_max = wall_radii[wall_radii.length() - 1]
  let mut min_c = cols
  let mut max_c = 0
  let mut min_r = rows
  let mut max_r = 0
  let mut has_outer = false
  for r in 0..<rows {
    for c in 0..<cols {
      let dist = hex_dist(c, r, cx, cy)
      if dist > outer_max && grid.get(c, r) == @types.Floor {
        has_outer = true
        if c < min_c {
          min_c = c
        }
        if c > max_c {
          max_c = c
        }
        if r < min_r {
          min_r = r
        }
        if r > max_r {
          max_r = r
        }
      }
    }
  }
  if has_outer {
    rooms.push(
      {
        rect: { x: min_c, y: min_r, w: max_c - min_c + 1, h: max_r - min_r + 1 },
        id: room_id,
      },
    )
  }
  { grid, rooms }
}
