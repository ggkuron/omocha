///| Delaunay triangulation + Gabriel Graph dungeon generation.
///  Random points are placed, triangulated with incremental Delaunay insertion,
///  then pruned to the Gabriel subgraph. Rooms are placed at each point and
///  corridors carved along Gabriel edges.

///|
pub(all) struct DelaunayConfig {
  width : Int
  height : Int
  num_points : Int
  room_min_size : Int
  room_max_size : Int
  corridor_width : Int
  extra_edge_chance : Double
}

///|
pub fn DelaunayConfig::default() -> DelaunayConfig {
  {
    width: 50,
    height: 25,
    num_points: 15,
    room_min_size: 3,
    room_max_size: 6,
    corridor_width: 1,
    extra_edge_chance: 0.15,
  }
}

///|
pub(all) struct DelaunayResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| A 2D point with double-precision coordinates.
priv struct DPoint {
  x : Double
  y : Double
}

///| A triangle referencing three vertex indices.
priv struct Triangle {
  v0 : Int
  v1 : Int
  v2 : Int
  mut alive : Bool
}

///| An edge between two vertex indices.
priv struct DEdge {
  a : Int
  b : Int
}

///|
fn rand_double(rng : @types.RNG) -> Double {
  (rng.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() / 2147483647.0
}

///|
fn dist_sq(a : DPoint, b : DPoint) -> Double {
  let dx = a.x - b.x
  let dy = a.y - b.y
  dx * dx + dy * dy
}

///|
fn abs_int(x : Int) -> Int {
  if x < 0 {
    -x
  } else {
    x
  }
}

///| Test whether point d lies inside the circumscribed circle of triangle (a, b, c).
///  Uses the standard determinant test. Returns true if d is strictly inside.
fn in_circumcircle(
  a : DPoint,
  b : DPoint,
  c : DPoint,
  d : DPoint
) -> Bool {
  let ax = a.x - d.x
  let ay = a.y - d.y
  let bx = b.x - d.x
  let by = b.y - d.y
  let cx = c.x - d.x
  let cy = c.y - d.y
  let det = ax * (by * (cx * cx + cy * cy) - cy * (bx * bx + by * by)) - bx *
    (ay * (cx * cx + cy * cy) - cy * (ax * ax + ay * ay)) + cx *
    (ay * (bx * bx + by * by) - by * (ax * ax + ay * ay))
  det > 0.0
}

///| Compute the signed area of triangle (a, b, c). Positive if CCW.
fn cross2d(a : DPoint, b : DPoint, c : DPoint) -> Double {
  (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

///| Perform incremental Delaunay triangulation.
///  Returns the list of triangles (some may be dead) and the points array.
fn delaunay_triangulate(points : Array[DPoint]) -> Array[Triangle] {
  let n = points.length()
  if n < 3 {
    return []
  }
  // Create a super-triangle that contains all points
  let mut min_x = points[0].x
  let mut max_x = points[0].x
  let mut min_y = points[0].y
  let mut max_y = points[0].y
  for p in points {
    if p.x < min_x {
      min_x = p.x
    }
    if p.x > max_x {
      max_x = p.x
    }
    if p.y < min_y {
      min_y = p.y
    }
    if p.y > max_y {
      max_y = p.y
    }
  }
  let dx = max_x - min_x + 1.0
  let dy = max_y - min_y + 1.0
  let dmax = if dx > dy { dx } else { dy }
  let mid_x = (min_x + max_x) / 2.0
  let mid_y = (min_y + max_y) / 2.0
  // Super-triangle vertices at indices n, n+1, n+2
  points.push({ x: mid_x - 20.0 * dmax, y: mid_y - dmax })
  points.push({ x: mid_x, y: mid_y + 20.0 * dmax })
  points.push({ x: mid_x + 20.0 * dmax, y: mid_y - dmax })
  let triangles : Array[Triangle] = []
  triangles.push({ v0: n, v1: n + 1, v2: n + 2, alive: true })
  // Insert each point
  for i in 0..<n {
    let p = points[i]
    // Find all triangles whose circumcircle contains p
    let bad_triangles : Array[Int] = []
    for t_idx in 0..<triangles.length() {
      let tri = triangles[t_idx]
      if not(tri.alive) {
        continue
      }
      if in_circumcircle(points[tri.v0], points[tri.v1], points[tri.v2], p) {
        bad_triangles.push(t_idx)
      }
    }
    // Find the boundary polygon (edges that are not shared by two bad triangles)
    let boundary : Array[DEdge] = []
    for bt in bad_triangles {
      let tri = triangles[bt]
      let edges : Array[DEdge] = [
        { a: tri.v0, b: tri.v1 },
        { a: tri.v1, b: tri.v2 },
        { a: tri.v2, b: tri.v0 },
      ]
      for e in edges {
        let mut shared = false
        for bt2 in bad_triangles {
          if bt2 == bt {
            continue
          }
          let tri2 = triangles[bt2]
          let e2s : Array[DEdge] = [
            { a: tri2.v0, b: tri2.v1 },
            { a: tri2.v1, b: tri2.v2 },
            { a: tri2.v2, b: tri2.v0 },
          ]
          for e2 in e2s {
            if (e.a == e2.a && e.b == e2.b) || (e.a == e2.b && e.b == e2.a) {
              shared = true
              break
            }
          }
          if shared {
            break
          }
        }
        if not(shared) {
          boundary.push(e)
        }
      }
    }
    // Remove bad triangles
    for bt in bad_triangles {
      triangles[bt].alive = false
    }
    // Create new triangles from boundary edges to the inserted point
    for e in boundary {
      // Ensure CCW orientation
      let area = cross2d(points[e.a], points[e.b], points[i])
      if area > 0.0 {
        triangles.push({ v0: e.a, v1: e.b, v2: i, alive: true })
      } else {
        triangles.push({ v0: e.b, v1: e.a, v2: i, alive: true })
      }
    }
  }
  // Remove triangles that reference super-triangle vertices
  for t in triangles {
    if t.v0 >= n || t.v1 >= n || t.v2 >= n {
      t.alive = false
    }
  }
  // Remove the super-triangle vertices from points
  ignore(points.pop())
  ignore(points.pop())
  ignore(points.pop())
  triangles
}

///| Extract unique edges from alive triangles.
fn extract_edges(triangles : Array[Triangle]) -> Array[DEdge] {
  let edges : Array[DEdge] = []
  for t in triangles {
    if not(t.alive) {
      continue
    }
    let tri_edges : Array[DEdge] = [
      { a: t.v0.min(t.v1), b: t.v0.max(t.v1) },
      { a: t.v1.min(t.v2), b: t.v1.max(t.v2) },
      { a: t.v0.min(t.v2), b: t.v0.max(t.v2) },
    ]
    for e in tri_edges {
      let mut found = false
      for existing in edges {
        if existing.a == e.a && existing.b == e.b {
          found = true
          break
        }
      }
      if not(found) {
        edges.push(e)
      }
    }
  }
  edges
}

///| Filter Delaunay edges to only keep Gabriel Graph edges.
///  An edge (u, v) is in the Gabriel Graph if and only if no other point
///  lies inside the diametral circle (circle with u-v as diameter).
fn gabriel_filter(
  edges : Array[DEdge],
  points : Array[DPoint],
  rng : @types.RNG,
  extra_chance : Double
) -> Array[DEdge] {
  let result : Array[DEdge] = []
  let n = points.length()
  for e in edges {
    let u = points[e.a]
    let v = points[e.b]
    let cx = (u.x + v.x) / 2.0
    let cy = (u.y + v.y) / 2.0
    let r_sq = dist_sq(u, v) / 4.0
    let mut is_gabriel = true
    for k in 0..<n {
      if k == e.a || k == e.b {
        continue
      }
      let pk = points[k]
      let dx = pk.x - cx
      let dy = pk.y - cy
      if dx * dx + dy * dy < r_sq - 1.0e-9 {
        is_gabriel = false
        break
      }
    }
    if is_gabriel {
      result.push(e)
    } else if rng.rand_bool(extra_chance) {
      // Keep some non-Gabriel edges for variety
      result.push(e)
    }
  }
  result
}

///| Carve a wide corridor between two points on the grid.
fn carve_wide_corridor(
  grid : @types.Grid2D,
  x0 : Int,
  y0 : Int,
  x1 : Int,
  y1 : Int,
  half_width : Int,
  tile : @types.Tile
) -> Unit {
  // L-shaped corridor: horizontal then vertical
  let mut cx = x0
  while cx != x1 {
    for dy in -half_width..=half_width {
      if grid.in_bounds(cx, y0 + dy) {
        grid.set(cx, y0 + dy, tile)
      }
    }
    if cx < x1 {
      cx += 1
    } else {
      cx -= 1
    }
  }
  let mut cy = y0
  while cy != y1 {
    for dx in -half_width..=half_width {
      if grid.in_bounds(x1 + dx, cy) {
        grid.set(x1 + dx, cy, tile)
      }
    }
    if cy < y1 {
      cy += 1
    } else {
      cy -= 1
    }
  }
  // Final cell
  for dx in -half_width..=half_width {
    for dy in -half_width..=half_width {
      if grid.in_bounds(x1 + dx, y1 + dy) {
        grid.set(x1 + dx, y1 + dy, tile)
      }
    }
  }
}

///| Generate a dungeon using Delaunay triangulation pruned to the Gabriel Graph.
pub fn generate(config : DelaunayConfig, rng : @types.RNG) -> DelaunayResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // --- Step 1: Place random points with minimum distance ---
  let points : Array[DPoint] = []
  let min_dist = 4.0
  let max_attempts = 200
  for _ in 0..<config.num_points {
    let margin = config.room_max_size / 2 + 2
    let mut placed = false
    for _ in 0..<max_attempts {
      let px = margin.to_double() +
        rand_double(rng) * (config.width - 2 * margin).to_double()
      let py = margin.to_double() +
        rand_double(rng) * (config.height - 2 * margin).to_double()
      let candidate : DPoint = { x: px, y: py }
      let mut too_close = false
      for existing in points {
        if dist_sq(candidate, existing) < min_dist * min_dist {
          too_close = true
          break
        }
      }
      if not(too_close) {
        points.push(candidate)
        placed = true
        break
      }
    }
    if not(placed) {
      // Fallback
      let px = margin.to_double() +
        rand_double(rng) * (config.width - 2 * margin).to_double()
      let py = margin.to_double() +
        rand_double(rng) * (config.height - 2 * margin).to_double()
      points.push({ x: px, y: py })
    }
  }
  let n = points.length()
  if n < 2 {
    return { grid, rooms }
  }
  // --- Step 2: Delaunay triangulation ---
  let triangles = delaunay_triangulate(points)
  // --- Step 3: Extract edges and filter to Gabriel Graph ---
  let all_edges = extract_edges(triangles)
  let gabriel_edges = gabriel_filter(
    all_edges, points, rng, config.extra_edge_chance,
  )
  // --- Step 4: Place rooms at each point ---
  for i in 0..<n {
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let cx = points[i].x.to_int()
    let cy = points[i].y.to_int()
    let rx = (cx - rw / 2).clamp(min=1, max=config.width - rw - 1)
    let ry = (cy - rh / 2).clamp(min=1, max=config.height - rh - 1)
    let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: i })
  }
  // --- Step 5: Carve corridors along Gabriel Graph edges ---
  let half_w = (config.corridor_width - 1) / 2
  for e in gabriel_edges {
    let from_room = rooms[e.a]
    let to_room = rooms[e.b]
    let fc = from_room.rect.center()
    let tc = to_room.rect.center()
    if half_w > 0 {
      carve_wide_corridor(grid, fc.x, fc.y, tc.x, tc.y, half_w, @types.Corridor)
    } else {
      grid.carve_l_corridor(fc, tc, @types.Corridor)
    }
  }
  // --- Step 6: Ensure connectivity via MST fallback ---
  // Build adjacency from Gabriel edges
  let connected = Array::make(n, false)
  connected[0] = true
  let queue : Array[Int] = [0]
  let mut head = 0
  while head < queue.length() {
    let cur = queue[head]
    head += 1
    for e in gabriel_edges {
      if e.a == cur && not(connected[e.b]) {
        connected[e.b] = true
        queue.push(e.b)
      } else if e.b == cur && not(connected[e.a]) {
        connected[e.a] = true
        queue.push(e.a)
      }
    }
  }
  // Connect any unreached components
  for i in 0..<n {
    if not(connected[i]) {
      // Find nearest connected point
      let mut best_j = 0
      let mut best_dist = 1.0e18
      for j in 0..<n {
        if connected[j] {
          let d = dist_sq(points[i], points[j])
          if d < best_dist {
            best_dist = d
            best_j = j
          }
        }
      }
      connected[i] = true
      let fc = rooms[i].rect.center()
      let tc = rooms[best_j].rect.center()
      grid.carve_l_corridor(fc, tc, @types.Corridor)
    }
  }
  // --- Step 7: Place Start and Goal ---
  // Start at room 0, Goal at the farthest room
  let start_room = rooms[0]
  let mut max_dist = 0
  let mut goal_idx = 0
  for i in 1..<n {
    let sc = start_room.rect.center()
    let rc = rooms[i].rect.center()
    let d = abs_int(sc.x - rc.x) + abs_int(sc.y - rc.y)
    if d > max_dist {
      max_dist = d
      goal_idx = i
    }
  }
  let sc = start_room.rect.center()
  grid.set(sc.x, sc.y, @types.Start)
  let gc = rooms[goal_idx].rect.center()
  grid.set(gc.x, gc.y, @types.Goal)
  { grid, rooms }
}
