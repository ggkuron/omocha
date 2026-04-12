///| Penrose tiling dungeon generation using de Bruijn's multigrid method.
///  Five families of parallel lines at 72-degree intervals create a
///  quasiperiodic pattern. Intersection regions are classified as thick or
///  thin rhombuses and mapped onto a discrete grid.

///|
pub(all) struct PenroseConfig {
  width : Int
  height : Int
  num_lines : Int
  floor_chance : Double
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn PenroseConfig::default() -> PenroseConfig {
  {
    width: 50,
    height: 25,
    num_lines: 4,
    floor_chance: 0.6,
    room_chance: 0.1,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct PenroseResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| A vertex produced by the multigrid method.
priv struct GridVertex {
  gx : Double
  gy : Double
  kind : Int // 0 = thick rhombus region, 1 = thin
}

///|
fn rand_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
fn abs_double(x : Double) -> Double {
  if x < 0.0 {
    -x
  } else {
    x
  }
}

///| Generate a Penrose-tiling dungeon.
///
///  The algorithm uses de Bruijn's multigrid (5-grid) method:
///  1. Create 5 families of parallel lines, each rotated by 72 degrees.
///     Each family has `num_lines` lines with random offsets.
///  2. Every pair of lines from *different* families intersects at a point.
///  3. At each intersection we compute integer "strip indices" for each
///     family and derive a 2D grid position from those indices.
///  4. The sum of strip indices modulo 2 classifies the vertex as belonging
///     to a thick or thin rhombus, which determines whether it becomes
///     Floor or Wall.
///  5. Optional rectangular rooms are scattered over Floor regions.
pub fn generate(config : PenroseConfig, rng : @types.RNG) -> PenroseResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // --- Step 1: Build the 5 families of parallel lines ---
  // Each family k has direction angle theta_k = k * 72 degrees.
  // A line in family k is defined by its signed distance from the origin.
  let num_families = 5
  let offsets : Array[Array[Double]] = []
  for _ in 0..<num_families {
    let family : Array[Double] = []
    for _ in 0..<config.num_lines {
      family.push(rand_double(rng) * 2.0 - 1.0)
    }
    offsets.push(family)
  }
  // Precompute direction vectors (cos, sin) for each family
  let dir_x : Array[Double] = []
  let dir_y : Array[Double] = []
  for k in 0..<num_families {
    let angle = k.to_double() * 2.0 * @math.PI / 5.0
    dir_x.push(@math.cos(angle))
    dir_y.push(@math.sin(angle))
  }
  // --- Step 2: Find intersections of lines from different families ---
  let vertices : Array[GridVertex] = []
  for k1 in 0..<num_families {
    for k2 = k1 + 1; k2 < num_families; {
      for i1 in 0..<config.num_lines {
        for i2 in 0..<config.num_lines {
          // Line from family k1: dot(normal_k1, p) = offset[k1][i1]
          // normal_k1 = (-sin(theta_k1), cos(theta_k1))
          // Similarly for k2.
          let nx1 = -dir_y[k1]
          let ny1 = dir_x[k1]
          let nx2 = -dir_y[k2]
          let ny2 = dir_x[k2]
          let det = nx1 * ny2 - nx2 * ny1
          if abs_double(det) < 1.0e-9 {
            continue i2 + 1
          }
          let d1 = offsets[k1][i1]
          let d2 = offsets[k2][i2]
          let ix = (d1 * ny2 - d2 * ny1) / det
          let iy = (nx1 * d2 - nx2 * d1) / det
          // Compute strip indices for every family at this intersection
          let mut index_sum = 0
          for k in 0..<num_families {
            // Project intersection onto normal of family k
            let proj = -dir_y[k] * ix + dir_x[k] * iy
            // Determine which strip this falls in
            let mut strip = 0
            for j in 0..<config.num_lines {
              if proj > offsets[k][j] {
                strip += 1
              }
            }
            index_sum += strip
          }
          let kind = index_sum % 2
          vertices.push({ gx: ix, gy: iy, kind })
        }
      }
      continue k2 + 1
    }
  }
  if vertices.length() == 0 {
    return { grid, rooms }
  }
  // --- Step 3: Map vertices to grid coordinates ---
  let mut min_gx = vertices[0].gx
  let mut max_gx = vertices[0].gx
  let mut min_gy = vertices[0].gy
  let mut max_gy = vertices[0].gy
  for v in vertices {
    if v.gx < min_gx {
      min_gx = v.gx
    }
    if v.gx > max_gx {
      max_gx = v.gx
    }
    if v.gy < min_gy {
      min_gy = v.gy
    }
    if v.gy > max_gy {
      max_gy = v.gy
    }
  }
  let range_x = max_gx - min_gx
  let range_y = max_gy - min_gy
  let scale_x = if range_x > 1.0e-9 {
    (config.width - 3).to_double() / range_x
  } else {
    1.0
  }
  let scale_y = if range_y > 1.0e-9 {
    (config.height - 3).to_double() / range_y
  } else {
    1.0
  }
  // --- Step 4: Paint vertices onto the grid ---
  // Use a density map: each cell accumulates how many vertices land on it,
  // and what "kind" they predominantly are.
  let count_map = Array::make(config.width * config.height, 0)
  let kind_map = Array::make(config.width * config.height, 0)
  for v in vertices {
    let px = ((v.gx - min_gx) * scale_x).to_int() + 1
    let py = ((v.gy - min_gy) * scale_y).to_int() + 1
    if px >= 0 && px < config.width && py >= 0 && py < config.height {
      let idx = py * config.width + px
      count_map[idx] = count_map[idx] + 1
      kind_map[idx] = kind_map[idx] + v.kind
    }
  }
  // Mark cells that received vertices
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      if count_map[idx] > 0 {
        // Thick rhombus (kind 0 dominant) -> Floor, thin -> Wall
        let dominant_kind = if kind_map[idx] * 2 > count_map[idx] {
          1
        } else {
          0
        }
        if dominant_kind == 0 {
          grid.set(x, y, @types.Floor)
        } else if rng.rand_bool(config.floor_chance) {
          grid.set(x, y, @types.Floor)
        }
      }
    }
  }
  // --- Step 5: Flood-fill to connect nearby floor cells ---
  // For each floor cell, also make neighbours floor if they are near a vertex
  // This makes the pattern more dungeon-like (connected regions).
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      if grid.get(x, y) == @types.Floor {
        // Expand to cardinal neighbours that are Wall
        let dirs : Array[(Int, Int)] = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for d in dirs {
          let nx = x + d.0
          let ny = y + d.1
          if grid.in_bounds(nx, ny) &&
            grid.get(nx, ny) == @types.Wall &&
            nx > 0 &&
            nx < config.width - 1 &&
            ny > 0 &&
            ny < config.height - 1 {
            // Expand with some probability
            if rng.rand_bool(config.floor_chance * 0.5) {
              grid.set(nx, ny, @types.Floor)
            }
          }
        }
      }
    }
  }
  // --- Step 6: Place rectangular rooms ---
  let max_room_attempts = config.width * config.height / 10
  for _ in 0..<max_room_attempts {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(1, config.width - rw - 1)
    let ry = rng.rand_range(1, config.height - rh - 1)
    // Check overlap with existing rooms
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    let mut overlaps = false
    for r in rooms {
      if rect.intersects(r.rect) {
        overlaps = true
        break
      }
    }
    if overlaps {
      continue
    }
    // Check that at least one cell in this rect is already Floor (connected)
    let mut touches_floor = false
    for cy in ry..<(ry + rh) {
      for cx in rx..<(rx + rw) {
        if grid.get(cx, cy) == @types.Floor {
          touches_floor = true
        }
      }
    }
    if not(touches_floor) {
      continue
    }
    // Carve the room
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: rooms.length() })
  }
  // --- Step 7: Connect isolated floor regions via corridors ---
  // Simple: connect each room center to the next
  for i in 1..<rooms.length() {
    let from = rooms[i - 1].rect.center()
    let to = rooms[i].rect.center()
    grid.carve_l_corridor(from, to, @types.Corridor)
  }
  // --- Step 8: Place Start and Goal ---
  if rooms.length() >= 2 {
    let start_center = rooms[0].rect.center()
    grid.set(start_center.x, start_center.y, @types.Start)
    let goal_center = rooms[rooms.length() - 1].rect.center()
    grid.set(goal_center.x, goal_center.y, @types.Goal)
  } else {
    // Find any floor cell for start / goal
    let mut placed_start = false
    let mut placed_goal = false
    for y in 1..<(config.height - 1) {
      for x in 1..<(config.width - 1) {
        if grid.get(x, y) == @types.Floor {
          if not(placed_start) {
            grid.set(x, y, @types.Start)
            placed_start = true
          } else if not(placed_goal) {
            grid.set(x, y, @types.Goal)
            placed_goal = true
          }
        }
      }
    }
  }
  { grid, rooms }
}
