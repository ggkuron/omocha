///|
pub(all) struct TinyKeepConfig {
  width : Int
  height : Int
  num_rooms : Int
  room_min_size : Int
  room_max_size : Int
  main_room_threshold : Int
  extra_edge_chance : Double
  separation_iterations : Int
}

///|
pub fn TinyKeepConfig::default() -> TinyKeepConfig {
  {
    width: 60,
    height: 35,
    num_rooms: 30,
    room_min_size: 3,
    room_max_size: 9,
    main_room_threshold: 5,
    extra_edge_chance: 0.15,
    separation_iterations: 50,
  }
}

///|
pub(all) struct TinyKeepResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
priv struct RoomData {
  mut x : Int
  mut y : Int
  w : Int
  h : Int
  mut is_main : Bool
}

///|
pub fn generate(config : TinyKeepConfig, rng : @types.RNG) -> TinyKeepResult {
  // Phase 1: Scatter rooms around center
  let cx = config.width / 2
  let cy = config.height / 2
  let scatter : Array[RoomData] = []
  for _ in 0..<config.num_rooms {
    let w = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let h = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    // Random position near center (uniform within a radius)
    let radius = config.width.min(config.height) / 4
    let rx = cx + rng.rand_range(-radius, radius + 1)
    let ry = cy + rng.rand_range(-radius, radius + 1)
    scatter.push({ x: rx - w / 2, y: ry - h / 2, w, h, is_main: false })
  }
  // Phase 2: Separation steering
  for _ in 0..<config.separation_iterations {
    let mut any_moved = false
    for i in 0..<scatter.length() {
      let mut dx = 0
      let mut dy = 0
      let ri = to_rect(scatter[i])
      for j in 0..<scatter.length() {
        if i == j {
          continue
        }
        let padded : @types.Rect = {
          x: scatter[j].x - 1,
          y: scatter[j].y - 1,
          w: scatter[j].w + 2,
          h: scatter[j].h + 2,
        }
        if ri.intersects(padded) {
          let cxi = scatter[i].x + scatter[i].w / 2
          let cyi = scatter[i].y + scatter[i].h / 2
          let cxj = scatter[j].x + scatter[j].w / 2
          let cyj = scatter[j].y + scatter[j].h / 2
          if cxi <= cxj { dx -= 1 } else { dx += 1 }
          if cyi <= cyj { dy -= 1 } else { dy += 1 }
        }
      }
      if dx != 0 || dy != 0 {
        scatter[i].x += sign(dx)
        scatter[i].y += sign(dy)
        any_moved = true
      }
    }
    if not(any_moved) {
      break
    }
  }
  // Clamp to bounds
  for r in scatter {
    r.x = r.x.clamp(min=1, max=config.width - r.w - 1)
    r.y = r.y.clamp(min=1, max=config.height - r.h - 1)
  }
  // Phase 3: Select main rooms (above threshold size)
  for r in scatter {
    if r.w >= config.main_room_threshold && r.h >= config.main_room_threshold {
      r.is_main = true
    }
  }
  // Collect main rooms
  let main_rooms : Array[Int] = []
  for i in 0..<scatter.length() {
    if scatter[i].is_main {
      main_rooms.push(i)
    }
  }
  // Need at least 2 main rooms
  if main_rooms.length() < 2 {
    // Promote largest rooms
    let sizes : Array[(Int, Int)] = []
    for i in 0..<scatter.length() {
      sizes.push((scatter[i].w * scatter[i].h, i))
    }
    // Sort by size descending (simple bubble sort)
    for i in 0..<sizes.length() {
      for j in (i + 1)..<sizes.length() {
        if sizes[j].0 > sizes[i].0 {
          sizes.swap(i, j)
        }
      }
    }
    main_rooms.clear()
    let count = scatter.length().min(5)
    for i in 0..<count {
      scatter[sizes[i].1].is_main = true
      main_rooms.push(sizes[i].1)
    }
  }
  // Phase 4: Delaunay approximation via sorted neighbor triangulation
  // Use simpler approach: connect each main room to its nearest neighbors
  let edges : Array[@types.Edge] = []
  let mst_edges = build_mst(scatter, main_rooms)
  for e in mst_edges {
    edges.push(e)
  }
  // Add extra edges between nearby main rooms
  for i in 0..<main_rooms.length() {
    for j in (i + 1)..<main_rooms.length() {
      let a = main_rooms[i]
      let b = main_rooms[j]
      let mut already = false
      for e in edges {
        if (e.from == a && e.to == b) || (e.from == b && e.to == a) {
          already = true
          break
        }
      }
      if not(already) && rng.rand_bool(config.extra_edge_chance) {
        edges.push({ from: a, to: b })
      }
    }
  }
  // Phase 5: Render grid
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve main rooms
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for i in main_rooms {
    let rect = to_rect(scatter[i])
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: room_id })
    room_id += 1
  }
  // Carve corridors
  for e in edges {
    let ca = rect_center(scatter[e.from])
    let cb = rect_center(scatter[e.to])
    grid.carve_l_corridor(ca, cb, @types.Corridor)
    // Also carve any small rooms that corridors pass through
  }
  // Carve small rooms that intersect corridors
  for i in 0..<scatter.length() {
    if scatter[i].is_main {
      continue
    }
    let rect = to_rect(scatter[i])
    // Check if any cell of this room is already carved
    let mut touches_corridor = false
    for y in rect.y..<(rect.y + rect.h) {
      for x in rect.x..<(rect.x + rect.w) {
        if grid.in_bounds(x, y) && grid.get(x, y) == @types.Corridor {
          touches_corridor = true
        }
      }
    }
    if touches_corridor {
      grid.fill_rect(rect, @types.Floor)
      rooms.push({ rect, id: room_id })
      room_id += 1
    }
  }
  // Remap edges to room indices
  let result_edges : Array[@types.Edge] = []
  for i in 0..<rooms.length().min(main_rooms.length()) {
    for e in edges {
      if e.from == main_rooms[i] {
        for j in 0..<rooms.length().min(main_rooms.length()) {
          if e.to == main_rooms[j] {
            result_edges.push({ from: i, to: j })
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
  { grid, rooms, edges: result_edges }
}

///|
fn to_rect(r : RoomData) -> @types.Rect {
  { x: r.x, y: r.y, w: r.w, h: r.h }
}

///|
fn rect_center(r : RoomData) -> @types.Point {
  { x: r.x + r.w / 2, y: r.y + r.h / 2 }
}

///|
fn sign(x : Int) -> Int {
  if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn build_mst(
  scatter : Array[RoomData],
  main_rooms : Array[Int]
) -> Array[@types.Edge] {
  let n = main_rooms.length()
  if n <= 1 {
    return []
  }
  let in_tree = Array::make(n, false)
  in_tree[0] = true
  let edges : Array[@types.Edge] = []
  let mut tree_size = 1
  while tree_size < n {
    let mut best_dist = 999999
    let mut best_from = 0
    let mut best_to = 0
    for i in 0..<n {
      if not(in_tree[i]) {
        continue
      }
      for j in 0..<n {
        if in_tree[j] {
          continue
        }
        let a = main_rooms[i]
        let b = main_rooms[j]
        let ca = rect_center(scatter[a])
        let cb = rect_center(scatter[b])
        let dist = abs(ca.x - cb.x) + abs(ca.y - cb.y)
        if dist < best_dist {
          best_dist = dist
          best_from = i
          best_to = j
        }
      }
    }
    in_tree[best_to] = true
    edges.push({ from: main_rooms[best_from], to: main_rooms[best_to] })
    tree_size += 1
  }
  edges
}
