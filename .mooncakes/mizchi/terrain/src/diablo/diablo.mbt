///|
pub(all) struct DiabloConfig {
  width : Int
  height : Int
  spine_length : Int
  branch_chance : Double
  loop_chance : Double
  room_min_size : Int
  room_max_size : Int
  room_padding : Int
  max_placement_attempts : Int
}

///|
pub fn DiabloConfig::default() -> DiabloConfig {
  {
    width: 60,
    height: 35,
    spine_length: 6,
    branch_chance: 0.5,
    loop_chance: 0.25,
    room_min_size: 3,
    room_max_size: 9,
    room_padding: 2,
    max_placement_attempts: 50,
  }
}

///|
pub(all) struct DiabloResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///|
priv enum RoomRole {
  Entrance
  Normal
  Boss
  Treasure
} derive(Eq)

///|
priv struct DiabloNode {
  id : Int
  role : RoomRole
  neighbors : Array[Int]
}

// --- Phase 1: Abstract graph generation ---

///|
fn build_graph(
  spine_length : Int,
  branch_chance : Double,
  loop_chance : Double,
  rng : @types.RNG
) -> (Array[DiabloNode], Array[@types.Edge]) {
  let nodes : Array[DiabloNode] = []
  let edges : Array[@types.Edge] = []
  // Create spine: Entrance -> Normal... -> Boss
  for i in 0..<spine_length {
    let role = if i == 0 {
      Entrance
    } else if i == spine_length - 1 {
      Boss
    } else {
      Normal
    }
    nodes.push({ id: i, role, neighbors: [] })
  }
  // Connect spine linearly
  for i in 0..<(spine_length - 1) {
    nodes[i].neighbors.push(i + 1)
    nodes[i + 1].neighbors.push(i)
    edges.push({ from: i, to: i + 1 })
  }
  // Add branches from Normal rooms
  for i in 1..<(spine_length - 1) {
    if rng.rand_bool(branch_chance) {
      let tid = nodes.length()
      nodes.push({ id: tid, role: Treasure, neighbors: [] })
      nodes[i].neighbors.push(tid)
      nodes[tid].neighbors.push(i)
      edges.push({ from: i, to: tid })
      // Possibly add a second treasure room
      if rng.rand_bool(branch_chance * 0.5) {
        let tid2 = nodes.length()
        nodes.push({ id: tid2, role: Treasure, neighbors: [] })
        nodes[i].neighbors.push(tid2)
        nodes[tid2].neighbors.push(i)
        edges.push({ from: i, to: tid2 })
      }
    }
  }
  // Add loop connections: branch endpoints back to nearby spine rooms
  for i in spine_length..<nodes.length() {
    if rng.rand_bool(loop_chance) {
      // Find which spine node this branch connects to
      let parent_spine = nodes[i].neighbors[0]
      // Pick a nearby spine node (within +-2 hops)
      let lo = (parent_spine - 2).clamp(min=0, max=spine_length - 1)
      let hi = (parent_spine + 2).clamp(min=0, max=spine_length - 1)
      let target = rng.rand_range(lo, hi + 1)
      // Avoid connecting to the same parent or creating duplicate edges
      if target != parent_spine {
        let mut already = false
        for n in nodes[i].neighbors {
          if n == target {
            already = true
            break
          }
        }
        if not(already) {
          nodes[i].neighbors.push(target)
          nodes[target].neighbors.push(i)
          edges.push({ from: i, to: target })
        }
      }
    }
  }
  (nodes, edges)
}

// --- Phase 2: Spatial placement ---

///|
fn room_size_for_role(
  role : RoomRole,
  min_size : Int,
  max_size : Int,
  rng : @types.RNG
) -> (Int, Int) {
  match role {
    Boss => {
      let s = max_size
      (s, s)
    }
    Entrance => {
      let s = (min_size + max_size) / 2
      (s, s)
    }
    Normal => {
      let w = rng.rand_range(min_size, max_size)
      let h = rng.rand_range(min_size, max_size)
      (w, h)
    }
    Treasure => (min_size, min_size)
  }
}

///|
fn place_rooms(
  nodes : Array[DiabloNode],
  config : DiabloConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  // BFS order starting from node 0
  let order : Array[Int] = []
  let visited = Array::make(nodes.length(), false)
  let queue : Array[Int] = [0]
  visited[0] = true
  while queue.length() > 0 {
    let cur = queue.unsafe_pop()
    order.push(cur)
    for n in nodes[cur].neighbors {
      if not(visited[n]) {
        visited[n] = true
        queue.push(n)
      }
    }
  }
  // Track placed rects for collision detection
  let placed : Array[@types.Rect] = []
  // Place entrance near center
  let (ew, eh) = room_size_for_role(
    nodes[0].role,
    config.room_min_size,
    config.room_max_size,
    rng,
  )
  let entrance_rect : @types.Rect = {
    x: config.width / 2 - ew / 2,
    y: config.height / 2 - eh / 2,
    w: ew,
    h: eh,
  }
  rooms.push({ rect: entrance_rect, id: 0 })
  placed.push(entrance_rect)
  // Place remaining rooms in BFS order
  for idx in 1..<order.length() {
    let node_id = order[idx]
    let (rw, rh) = room_size_for_role(
      nodes[node_id].role,
      config.room_min_size,
      config.room_max_size,
      rng,
    )
    // Find a placed neighbor to use as anchor
    let mut anchor_rect : @types.Rect = entrance_rect
    for n in nodes[node_id].neighbors {
      // Check if neighbor is already placed
      for r in rooms {
        if r.id == n {
          anchor_rect = r.rect
          break
        }
      }
    }
    let anchor_center = anchor_rect.center()
    let mut best_rect : @types.Rect = { x: 1, y: 1, w: rw, h: rh }
    let mut found = false
    for _ in 0..<config.max_placement_attempts {
      // Random direction and distance from anchor
      let dir = rng.rand_direction()
      let dist = rng.rand_range(
        rw.max(rh) + config.room_padding + 2,
        rw.max(rh) + config.room_padding + 8,
      )
      let cx = (anchor_center.x + dir.x * dist).clamp(
        min=1 + rw / 2,
        max=config.width - 1 - rw / 2 - rw % 2,
      )
      let cy = (anchor_center.y + dir.y * dist).clamp(
        min=1 + rh / 2,
        max=config.height - 1 - rh / 2 - rh % 2,
      )
      let candidate : @types.Rect = { x: cx - rw / 2, y: cy - rh / 2, w: rw, h: rh }
      // Check bounds
      if candidate.x < 1 ||
        candidate.y < 1 ||
        candidate.x + candidate.w >= config.width - 1 ||
        candidate.y + candidate.h >= config.height - 1 {
        continue
      }
      // Check collision with placed rooms (with padding)
      let mut collides = false
      for p in placed {
        let padded : @types.Rect = {
          x: p.x - config.room_padding,
          y: p.y - config.room_padding,
          w: p.w + config.room_padding * 2,
          h: p.h + config.room_padding * 2,
        }
        if candidate.intersects(padded) {
          collides = true
          break
        }
      }
      if not(collides) {
        best_rect = candidate
        found = true
        break
      }
    }
    if found {
      rooms.push({ rect: best_rect, id: node_id })
      placed.push(best_rect)
    } else {
      // Fallback: place at a shifted position from anchor
      let fallback : @types.Rect = {
        x: (anchor_center.x + rng.rand_range(-3, 4)).clamp(
          min=1,
          max=config.width - rw - 1,
        ),
        y: (anchor_center.y + rng.rand_range(-3, 4)).clamp(
          min=1,
          max=config.height - rh - 1,
        ),
        w: rw,
        h: rh,
      }
      rooms.push({ rect: fallback, id: node_id })
      placed.push(fallback)
    }
  }
  rooms
}

// --- Phase 3: Grid rendering ---

///|
fn render_to_grid(
  nodes : Array[DiabloNode],
  rooms : Array[@types.Room],
  edges : Array[@types.Edge],
  config : DiabloConfig
) -> @types.Grid2D {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Build id -> room lookup
  let room_by_id : Array[@types.Room?] = Array::make(nodes.length(), None)
  for r in rooms {
    room_by_id[r.id] = Some(r)
  }
  // Carve rooms
  for r in rooms {
    grid.fill_rect(r.rect, @types.Floor)
  }
  // Carve corridors for each edge
  for edge in edges {
    let from_room = room_by_id[edge.from]
    let to_room = room_by_id[edge.to]
    match (from_room, to_room) {
      (Some(fr), Some(tr)) =>
        grid.carve_l_corridor(fr.rect.center(), tr.rect.center(), @types.Corridor)
      _ => ()
    }
  }
  // Place doors: scan room perimeters for Corridor adjacency
  for r in rooms {
    let rect = r.rect
    // Top and bottom edges
    for x in rect.x..<(rect.x + rect.w) {
      // Top edge
      if rect.y > 0 && grid.get(x, rect.y - 1) == @types.Corridor {
        grid.set(x, rect.y, @types.Door)
      }
      // Bottom edge
      let by = rect.y + rect.h - 1
      if by + 1 < config.height && grid.get(x, by + 1) == @types.Corridor {
        grid.set(x, by, @types.Door)
      }
    }
    // Left and right edges
    for y in rect.y..<(rect.y + rect.h) {
      // Left edge
      if rect.x > 0 && grid.get(rect.x - 1, y) == @types.Corridor {
        grid.set(rect.x, y, @types.Door)
      }
      // Right edge
      let rx = rect.x + rect.w - 1
      if rx + 1 < config.width && grid.get(rx + 1, y) == @types.Corridor {
        grid.set(rx, y, @types.Door)
      }
    }
  }
  // Place Start and Goal
  match room_by_id[0] {
    Some(r) => {
      let c = r.rect.center()
      grid.set(c.x, c.y, @types.Start)
    }
    _ => ()
  }
  // Boss room is the last spine node (id = spine_length - 1)
  // Find the node with Boss role
  for node in nodes {
    if node.role == Boss {
      match room_by_id[node.id] {
        Some(r) => {
          let c = r.rect.center()
          grid.set(c.x, c.y, @types.Goal)
        }
        _ => ()
      }
      break
    }
  }
  grid
}

///|
pub fn generate(config : DiabloConfig, rng : @types.RNG) -> DiabloResult {
  let (nodes, edges) = build_graph(
    config.spine_length,
    config.branch_chance,
    config.loop_chance,
    rng,
  )
  let rooms = place_rooms(nodes, config, rng)
  let grid = render_to_grid(nodes, rooms, edges, config)
  { grid, rooms, edges }
}
