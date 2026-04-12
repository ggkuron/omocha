///|
pub(all) struct MissionGraphConfig {
  width : Int
  height : Int
  num_nodes : Int
  branch_chance : Double
  lock_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn MissionGraphConfig::default() -> MissionGraphConfig {
  {
    width: 60,
    height: 35,
    num_nodes: 10,
    branch_chance: 0.3,
    lock_chance: 0.2,
    room_min_size: 4,
    room_max_size: 7,
  }
}

///|
pub(all) struct MissionGraphResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///| Internal node for the mission graph.
priv enum MissionNodeKind {
  MStart
  MGoal
  MObjective
  MKey
  MLock
}

///|
priv struct MissionNode {
  id : Int
  kind : MissionNodeKind
  children : Array[Int]
}

///| Phase 1: Generate a Dormans-style mission graph.
/// Linear chain with branches and lock-key pairs.
fn build_mission_graph(
  num_nodes : Int,
  branch_chance : Double,
  lock_chance : Double,
  rng : @types.RNG
) -> (Array[MissionNode], Array[@types.Edge]) {
  let nodes : Array[MissionNode] = []
  let edges : Array[@types.Edge] = []
  // Create main chain: Start -> Objective... -> Goal
  for i in 0..<num_nodes {
    let kind = if i == 0 {
      MStart
    } else if i == num_nodes - 1 {
      MGoal
    } else {
      MObjective
    }
    nodes.push({ id: i, kind, children: [] })
  }
  // Connect main chain
  for i in 0..<(num_nodes - 1) {
    nodes[i].children.push(i + 1)
    edges.push({ from: i, to: i + 1 })
  }
  // Add branches from objective nodes
  let branch_nodes : Array[Int] = []
  for i in 1..<(num_nodes - 1) {
    if rng.rand_bool(branch_chance) {
      let branch_id = nodes.length()
      nodes.push({ id: branch_id, kind: MObjective, children: [] })
      nodes[i].children.push(branch_id)
      edges.push({ from: i, to: branch_id })
      branch_nodes.push(branch_id)
      // Optionally add a second level branch
      if rng.rand_bool(branch_chance * 0.4) {
        let sub_id = nodes.length()
        nodes.push({ id: sub_id, kind: MObjective, children: [] })
        nodes[branch_id].children.push(sub_id)
        edges.push({ from: branch_id, to: sub_id })
        branch_nodes.push(sub_id)
      }
    }
  }
  // Insert lock-key pairs on main chain edges.
  // A lock blocks progress on the main chain; its key is placed at a branch point.
  for i in 2..<(num_nodes - 1) {
    if rng.rand_bool(lock_chance) {
      // Turn this main-chain node into a lock
      nodes[i] = { ..nodes[i], kind: MLock }
      // Place the key: prefer a branch node reachable from an earlier main-chain node
      let mut key_placed = false
      for b in branch_nodes {
        // Check if the branch is reachable from a node before the lock
        let parent = find_parent(nodes, b)
        if parent >= 0 && parent < i {
          match nodes[b].kind {
            MObjective => {
              nodes[b] = { ..nodes[b], kind: MKey }
              key_placed = true
              break
            }
            _ => ()
          }
        }
      }
      // If no branch available, create a new key side-room from an earlier node
      if not(key_placed) {
        let key_parent = rng.rand_range(0, i)
        let key_id = nodes.length()
        nodes.push({ id: key_id, kind: MKey, children: [] })
        nodes[key_parent].children.push(key_id)
        edges.push({ from: key_parent, to: key_id })
      }
    }
  }
  (nodes, edges)
}

///| Find the parent node ID for a given child node in the graph.
fn find_parent(nodes : Array[MissionNode], child_id : Int) -> Int {
  for node in nodes {
    for c in node.children {
      if c == child_id {
        return node.id
      }
    }
  }
  -1
}

///| Phase 2: Assign spatial positions to each node using grid-based layout.
/// Nodes on the main chain are spread horizontally. Branches extend vertically.
fn layout_mission_rooms(
  nodes : Array[MissionNode],
  config : MissionGraphConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let margin = 2
  // Calculate grid positions for each node
  // Main chain nodes get x positions spread across the width
  // Branch nodes get positions offset vertically from their parent
  let positions : Array[@types.Point] = Array::make(
    nodes.length(),
    { x: 0, y: 0 },
  )
  // Find main chain length (nodes 0..num_main-1)
  let mut main_chain_len = 0
  for i in 0..<nodes.length() {
    match nodes[i].kind {
      MStart | MGoal | MObjective | MLock => {
        // Check if this is a main chain node (id < some threshold)
        // Main chain nodes have sequential ids starting from 0
        let is_main = i < nodes.length()
        // We need to check if this node is on the main chain
        // Main chain: node 0's children lead to node 1, etc.
        let mut on_main = false
        if i == 0 {
          on_main = true
        } else {
          // Check if any earlier main chain node has this as a child
          for j in 0..<i {
            if j < main_chain_len || j == 0 {
              for c in nodes[j].children {
                if c == i && i == j + 1 {
                  on_main = true
                  break
                }
              }
            }
            if on_main {
              break
            }
          }
        }
        ignore(is_main)
        if on_main {
          main_chain_len = i + 1
        }
      }
      MKey => ()
    }
  }
  // Determine main chain by following the linear path from node 0
  let main_chain : Array[Int] = []
  let mut current = 0
  let visited_main = Array::make(nodes.length(), false)
  while current >= 0 && current < nodes.length() && not(visited_main[current]) {
    main_chain.push(current)
    visited_main[current] = true
    // Find the next main chain node (lowest-id child that's id == current+1)
    let mut next = -1
    for c in nodes[current].children {
      if c == current + 1 {
        next = c
        break
      }
    }
    if next < 0 {
      break
    }
    current = next
  }
  // Layout main chain horizontally
  let x_spacing = if main_chain.length() > 1 {
    (config.width - margin * 2 - config.room_max_size) / (main_chain.length() - 1)
  } else {
    config.width / 2
  }
  for i, node_id in main_chain {
    let x = margin + i * x_spacing + config.room_max_size / 2
    let y = config.height / 2
    positions[node_id] = { x, y }
  }
  // Layout branch nodes: offset from their parent
  for i in 0..<nodes.length() {
    if visited_main[i] {
      continue
    }
    // Find parent
    let parent_id = find_parent(nodes, i)
    if parent_id >= 0 {
      let parent_pos = positions[parent_id]
      // Place above or below the parent
      let y_offset = if rng.rand_bool(0.5) {
        -(config.height / 4)
      } else {
        config.height / 4
      }
      let x_jitter = rng.rand_range(-2, 3)
      let px = (parent_pos.x + x_jitter).clamp(
        min=margin + config.room_max_size / 2,
        max=config.width - margin - config.room_max_size / 2,
      )
      let py = (parent_pos.y + y_offset).clamp(
        min=margin + config.room_max_size / 2,
        max=config.height - margin - config.room_max_size / 2,
      )
      positions[i] = { x: px, y: py }
    }
  }
  // Create rooms at calculated positions
  for i in 0..<nodes.length() {
    let pos = positions[i]
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = (pos.x - rw / 2).clamp(min=margin, max=config.width - rw - margin)
    let ry = (pos.y - rh / 2).clamp(min=margin, max=config.height - rh - margin)
    rooms.push({ rect: { x: rx, y: ry, w: rw, h: rh }, id: i })
  }
  // Attempt to resolve overlaps by jittering
  for _ in 0..<30 {
    let mut any_overlap = false
    for i in 0..<rooms.length() {
      for j in (i + 1)..<rooms.length() {
        let padded : @types.Rect = {
          x: rooms[j].rect.x - 1,
          y: rooms[j].rect.y - 1,
          w: rooms[j].rect.w + 2,
          h: rooms[j].rect.h + 2,
        }
        if rooms[i].rect.intersects(padded) {
          any_overlap = true
          // Push room j away
          let ci = rooms[i].rect.center()
          let cj = rooms[j].rect.center()
          let dx = if cj.x >= ci.x { 2 } else { -2 }
          let dy = if cj.y >= ci.y { 2 } else { -2 }
          let new_x = (rooms[j].rect.x + dx).clamp(
            min=margin,
            max=config.width - rooms[j].rect.w - margin,
          )
          let new_y = (rooms[j].rect.y + dy).clamp(
            min=margin,
            max=config.height - rooms[j].rect.h - margin,
          )
          rooms[j] = {
            ..rooms[j],
            rect: { ..rooms[j].rect, x: new_x, y: new_y },
          }
        }
      }
    }
    if not(any_overlap) {
      break
    }
  }
  rooms
}

///| Phase 3: Render the mission graph onto the grid.
fn render_mission_grid(
  nodes : Array[MissionNode],
  rooms : Array[@types.Room],
  edges : Array[@types.Edge],
  config : MissionGraphConfig
) -> @types.Grid2D {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve rooms
  for r in rooms {
    grid.fill_rect(r.rect, @types.Floor)
  }
  // Carve corridors following graph edges
  for edge in edges {
    if edge.from < rooms.length() && edge.to < rooms.length() {
      let from_center = rooms[edge.from].rect.center()
      let to_center = rooms[edge.to].rect.center()
      // Check if the target is a lock node
      let is_locked = if edge.to < nodes.length() {
        match nodes[edge.to].kind {
          MLock => true
          _ => false
        }
      } else {
        false
      }
      if is_locked {
        // Carve corridor with LockedDoor at bend point
        let mid : @types.Point = { x: to_center.x, y: from_center.y }
        grid.carve_line(from_center, mid, @types.Corridor)
        grid.carve_line(mid, to_center, @types.Corridor)
        grid.set(mid.x, mid.y, @types.LockedDoor)
      } else {
        grid.carve_l_corridor(from_center, to_center, @types.Corridor)
      }
    }
  }
  // Place special tiles
  for i in 0..<nodes.length() {
    if i < rooms.length() {
      let center = rooms[i].rect.center()
      match nodes[i].kind {
        MStart => grid.set(center.x, center.y, @types.Start)
        MGoal => grid.set(center.x, center.y, @types.Goal)
        MKey => grid.set(center.x, center.y, @types.Key)
        MLock => () // LockedDoor is placed in corridor
        MObjective => ()
      }
    }
  }
  grid
}

///|
pub fn generate(
  config : MissionGraphConfig,
  rng : @types.RNG
) -> MissionGraphResult {
  let (nodes, edges) = build_mission_graph(
    config.num_nodes,
    config.branch_chance,
    config.lock_chance,
    rng,
  )
  let rooms = layout_mission_rooms(nodes, config, rng)
  let grid = render_mission_grid(nodes, rooms, edges, config)
  { grid, rooms, edges }
}
