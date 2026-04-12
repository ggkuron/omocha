///|
pub(all) struct CyclicDungeonConfig {
  width : Int
  height : Int
  num_rooms : Int
  num_keys : Int
  cycle_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn CyclicDungeonConfig::default() -> CyclicDungeonConfig {
  {
    width: 50,
    height: 25,
    num_rooms: 8,
    num_keys: 2,
    cycle_chance: 0.3,
    room_min_size: 4,
    room_max_size: 7,
  }
}

///|
pub(all) struct CyclicDungeonResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///| Internal node for the cyclic dungeon graph
priv enum CyclicNodeKind {
  CStart
  CGoal
  CRoom
  CKey
  CLock
}

///|
priv struct CyclicNode {
  kind : CyclicNodeKind
  neighbors : Array[Int]
}

///| Phase 1: Build the cyclic graph with keys and locks.
/// Creates a linear chain, adds back-edges for cycles, then inserts key-lock pairs.
fn build_cyclic_graph(
  num_rooms : Int,
  num_keys : Int,
  cycle_chance : Double,
  rng : @types.RNG
) -> (Array[CyclicNode], Array[@types.Edge]) {
  let nodes : Array[CyclicNode] = []
  let edges : Array[@types.Edge] = []
  // Create linear chain: Start -> Room... -> Goal
  for i in 0..<num_rooms {
    let kind = if i == 0 {
      CStart
    } else if i == num_rooms - 1 {
      CGoal
    } else {
      CRoom
    }
    nodes.push({ kind, neighbors: [] })
  }
  // Connect linearly
  for i in 0..<(num_rooms - 1) {
    nodes[i].neighbors.push(i + 1)
    nodes[i + 1].neighbors.push(i)
    edges.push({ from: i, to: i + 1 })
  }
  // Add cycle back-edges: from a later node back to an earlier node
  for i in 3..<num_rooms {
    if rng.rand_bool(cycle_chance) {
      // Pick a target at least 2 steps back
      let target = rng.rand_range(0, i - 1)
      // Check for duplicate edge
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
  // Place key-lock pairs along the main chain.
  // Lock is placed on edge between node (lock_pos-1) and node lock_pos.
  // Key is placed on a node before lock_pos that is reachable without passing through the lock.
  let mut keys_placed = 0
  // Candidate lock positions: nodes 2..num_rooms-2 on the main chain
  let lock_candidates : Array[Int] = []
  for i in 2..<(num_rooms - 1) {
    lock_candidates.push(i)
  }
  rng.rand_shuffle(lock_candidates)
  for candidate in lock_candidates {
    if keys_placed >= num_keys {
      break
    }
    // Place lock at this chain position
    nodes[candidate] = { ..nodes[candidate], kind: CLock }
    // Place key at a node before the lock position (reachable without crossing the lock)
    let key_pos = rng.rand_range(0, candidate)
    // If key_pos is already occupied by something special, find the nearest free node
    let mut actual_key_pos = key_pos
    for j in 0..<candidate {
      let check = (key_pos + j) % candidate
      match nodes[check].kind {
        CRoom | CStart => {
          actual_key_pos = check
          break
        }
        _ => ()
      }
    }
    // Add key as a side room branching off the chosen node
    let key_node_id = nodes.length()
    nodes.push({
      kind: CKey,
      neighbors: [actual_key_pos],
    })
    nodes[actual_key_pos].neighbors.push(key_node_id)
    edges.push({ from: actual_key_pos, to: key_node_id })
    keys_placed += 1
  }
  (nodes, edges)
}

///| Phase 2: Place rooms on the grid, spreading them across the available space.
fn place_cyclic_rooms(
  nodes : Array[CyclicNode],
  config : CyclicDungeonConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let margin = 2
  for i in 0..<nodes.length() {
    ignore(i)
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let mut placed = false
    for _ in 0..<80 {
      let x = rng.rand_range(margin, config.width - rw - margin)
      let y = rng.rand_range(margin, config.height - rh - margin)
      let rect : @types.Rect = { x, y, w: rw, h: rh }
      let mut overlaps = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 2,
          y: existing.rect.y - 2,
          w: existing.rect.w + 4,
          h: existing.rect.h + 4,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        rooms.push({ rect, id: rooms.length() })
        placed = true
        break
      }
    }
    if not(placed) {
      // Fallback: place at a random position
      let x = rng.rand_range(margin, config.width - rw - margin)
      let y = rng.rand_range(margin, config.height - rh - margin)
      rooms.push({ rect: { x, y, w: rw, h: rh }, id: rooms.length() })
    }
  }
  rooms
}

///| Phase 3: Render rooms, corridors, keys, locks, start, and goal onto the grid.
fn render_cyclic_grid(
  nodes : Array[CyclicNode],
  rooms : Array[@types.Room],
  edges : Array[@types.Edge],
  config : CyclicDungeonConfig
) -> @types.Grid2D {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve rooms
  for r in rooms {
    grid.fill_rect(r.rect, @types.Floor)
  }
  // Carve corridors for each edge
  for edge in edges {
    if edge.from < rooms.length() && edge.to < rooms.length() {
      let from_center = rooms[edge.from].rect.center()
      let to_center = rooms[edge.to].rect.center()
      // Check if the destination node is a lock; if so, place LockedDoor at midpoint
      let is_locked = if edge.to < nodes.length() {
        match nodes[edge.to].kind {
          CLock => true
          _ => false
        }
      } else {
        false
      }
      if is_locked {
        carve_corridor_with_locked_door(grid, from_center, to_center)
      } else {
        grid.carve_l_corridor(from_center, to_center, @types.Corridor)
      }
    }
  }
  // Place special tiles based on node kinds
  for i in 0..<nodes.length() {
    if i < rooms.length() {
      let center = rooms[i].rect.center()
      match nodes[i].kind {
        CStart => grid.set(center.x, center.y, @types.Start)
        CGoal => grid.set(center.x, center.y, @types.Goal)
        CKey => grid.set(center.x, center.y, @types.Key)
        CLock => () // LockedDoor is placed in corridor
        CRoom => ()
      }
    }
  }
  grid
}

///| Carve an L-shaped corridor and place a LockedDoor at the midpoint.
fn carve_corridor_with_locked_door(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  let mid : @types.Point = { x: to.x, y: from.y }
  grid.carve_line(from, mid, @types.Corridor)
  grid.carve_line(mid, to, @types.Corridor)
  // Place locked door at the bend point
  grid.set(mid.x, mid.y, @types.LockedDoor)
}

///|
pub fn generate(
  config : CyclicDungeonConfig,
  rng : @types.RNG
) -> CyclicDungeonResult {
  let (nodes, edges) = build_cyclic_graph(
    config.num_rooms,
    config.num_keys,
    config.cycle_chance,
    rng,
  )
  let rooms = place_cyclic_rooms(nodes, config, rng)
  let grid = render_cyclic_grid(nodes, rooms, edges, config)
  { grid, rooms, edges }
}
