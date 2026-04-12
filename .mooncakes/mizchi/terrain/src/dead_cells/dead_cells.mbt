///|
pub(all) struct DeadCellsConfig {
  width : Int
  height : Int
  path_length : Int
  max_branches : Int
  branch_chance : Double
  shortcut_chance : Double
  biome_min_size : Int
  biome_max_size : Int
}

///|
pub fn DeadCellsConfig::default() -> DeadCellsConfig {
  {
    width: 60,
    height: 35,
    path_length: 5,
    max_branches: 3,
    branch_chance: 0.4,
    shortcut_chance: 0.2,
    biome_min_size: 6,
    biome_max_size: 10,
  }
}

///|
pub(all) struct DeadCellsResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| A biome node in the directed graph.
priv struct BiomeNode {
  col : Int
  row : Int
  mut biome_w : Int
  mut biome_h : Int
  connections : Array[Int]
}

///| Phase 1: Build a directed graph of biome nodes with branching paths.
fn build_biome_graph(
  path_length : Int,
  max_branches : Int,
  branch_chance : Double,
  rng : @types.RNG
) -> Array[BiomeNode] {
  let nodes : Array[BiomeNode] = []
  // Grid tracking which (col, row) slots are occupied: index = row * path_length + col
  let occupied : Array[Int] = Array::make(path_length * max_branches, -1)
  // -1 = unoccupied, otherwise node index
  // Start node at col=0, row=max_branches/2
  let start_row = max_branches / 2
  let start_node : BiomeNode = {
    col: 0,
    row: start_row,
    biome_w: 0,
    biome_h: 0,
    connections: [],
  }
  nodes.push(start_node)
  occupied[start_row * path_length + 0] = 0
  // For each column, track which nodes are at the frontier
  let frontier : Array[Int] = [0]
  // node indices at current frontier
  for col in 1..<path_length {
    let next_frontier : Array[Int] = []
    for node_idx in frontier {
      let node = nodes[node_idx]
      // Main path: always continue forward in same row
      let main_row = node.row
      if occupied[main_row * path_length + col] < 0 {
        let new_idx = nodes.length()
        let new_node : BiomeNode = {
          col,
          row: main_row,
          biome_w: 0,
          biome_h: 0,
          connections: [],
        }
        nodes.push(new_node)
        occupied[main_row * path_length + col] = new_idx
        node.connections.push(new_idx)
        next_frontier.push(new_idx)
      } else {
        // Slot already taken, just connect to it
        let existing_idx = occupied[main_row * path_length + col]
        node.connections.push(existing_idx)
      }
      // Branch: try to branch to an adjacent row
      if rng.rand_bool(branch_chance) {
        let branch_dir = if rng.rand_bool(0.5) { -1 } else { 1 }
        let branch_row = main_row + branch_dir
        if branch_row >= 0 && branch_row < max_branches {
          if occupied[branch_row * path_length + col] < 0 {
            let new_idx = nodes.length()
            let branch_node : BiomeNode = {
              col,
              row: branch_row,
              biome_w: 0,
              biome_h: 0,
              connections: [],
            }
            nodes.push(branch_node)
            occupied[branch_row * path_length + col] = new_idx
            node.connections.push(new_idx)
            next_frontier.push(new_idx)
          } else {
            let existing_idx = occupied[branch_row * path_length + col]
            // Connect to existing node (convergence)
            let mut already_connected = false
            for c in node.connections {
              if c == existing_idx {
                already_connected = true
                break
              }
            }
            if not(already_connected) {
              node.connections.push(existing_idx)
            }
          }
        }
      }
    }
    // If no nodes in next frontier (shouldn't happen), force one
    if next_frontier.length() == 0 {
      let force_row = start_row
      if occupied[force_row * path_length + col] < 0 {
        let new_idx = nodes.length()
        let forced : BiomeNode = {
          col,
          row: force_row,
          biome_w: 0,
          biome_h: 0,
          connections: [],
        }
        nodes.push(forced)
        occupied[force_row * path_length + col] = new_idx
        next_frontier.push(new_idx)
        // Connect from last frontier node
        if frontier.length() > 0 {
          nodes[frontier[frontier.length() - 1]].connections.push(new_idx)
        }
      }
    }
    // Replace frontier contents with next_frontier
    frontier.clear()
    for idx in next_frontier {
      frontier.push(idx)
    }
  }
  nodes
}

///| Phase 2: Assign sizes to each biome node.
fn assign_biome_sizes(
  nodes : Array[BiomeNode],
  biome_min_size : Int,
  biome_max_size : Int,
  rng : @types.RNG
) -> Unit {
  for node in nodes {
    node.biome_w = rng.rand_range(biome_min_size, biome_max_size + 1)
    node.biome_h = rng.rand_range(biome_min_size, biome_max_size + 1)
  }
}

///| Phase 3: Add shortcut connections (LockedDoor connections skipping columns).
fn add_shortcuts(
  nodes : Array[BiomeNode],
  shortcut_chance : Double,
  rng : @types.RNG
) -> Array[(Int, Int)] {
  let shortcuts : Array[(Int, Int)] = []
  for i, node in nodes {
    // Only consider shortcuts that skip at least one column
    for j, target in nodes {
      if target.col > node.col + 1 {
        let node_count = nodes.length().to_double()
        let divisor = if node_count > 1.0 { node_count } else { 1.0 }
        if rng.rand_bool(shortcut_chance / divisor) {
          // Add shortcut
          let mut already = false
          for c in node.connections {
            if c == j {
              already = true
              break
            }
          }
          if not(already) {
            shortcuts.push((i, j))
          }
        }
      }
    }
  }
  shortcuts
}

///| Phase 4: Layout biome rooms on the grid and render.
fn render_dead_cells_grid(
  nodes : Array[BiomeNode],
  shortcuts : Array[(Int, Int)],
  config : DeadCellsConfig
) -> (@types.Grid2D, Array[@types.Room]) {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  // Compute layout positions for each node
  // Divide width by path_length, height by max_branches
  let col_width = config.width / config.path_length
  let row_height = config.height / config.max_branches
  // Store computed positions
  let positions : Array[(Int, Int, Int, Int)] = []
  // (px, py, pw, ph)
  for node in nodes {
    let area_x = node.col * col_width
    let area_y = node.row * row_height
    // Center the biome in its grid cell
    let bw = node.biome_w.min(col_width - 2)
    let bh = node.biome_h.min(row_height - 2)
    let px = area_x + (col_width - bw) / 2
    let py = area_y + (row_height - bh) / 2
    positions.push((px, py, bw, bh))
  }
  // Carve biome rooms
  for i, _node in nodes {
    let pos = positions[i]
    let px = pos.0
    let py = pos.1
    let pw = pos.2
    let ph = pos.3
    let rect : @types.Rect = { x: px, y: py, w: pw, h: ph }
    grid.fill_rect(rect, @types.Floor)
    rooms.push({ rect, id: i })
  }
  // Draw corridors between connected nodes
  for i, node in nodes {
    let src_pos = positions[i]
    let src_cx = src_pos.0 + src_pos.2 / 2
    let src_cy = src_pos.1 + src_pos.3 / 2
    for conn_idx in node.connections {
      let dst_pos = positions[conn_idx]
      let dst_cx = dst_pos.0 + dst_pos.2 / 2
      let dst_cy = dst_pos.1 + dst_pos.3 / 2
      let from : @types.Point = { x: src_cx, y: src_cy }
      let to : @types.Point = { x: dst_cx, y: dst_cy }
      grid.carve_l_corridor(from, to, @types.Corridor)
      // Place Door at the midpoint to indicate direction
      let mid_x = (src_cx + dst_cx) / 2
      let mid_y = (src_cy + dst_cy) / 2
      if grid.in_bounds(mid_x, mid_y) {
        grid.set(mid_x, mid_y, @types.Door)
      }
    }
  }
  // Draw shortcut corridors with LockedDoor
  for shortcut in shortcuts {
    let src_idx = shortcut.0
    let dst_idx = shortcut.1
    let src_pos = positions[src_idx]
    let dst_pos = positions[dst_idx]
    let src_cx = src_pos.0 + src_pos.2 / 2
    let src_cy = src_pos.1 + src_pos.3 / 2
    let dst_cx = dst_pos.0 + dst_pos.2 / 2
    let dst_cy = dst_pos.1 + dst_pos.3 / 2
    let from : @types.Point = { x: src_cx, y: src_cy }
    let to : @types.Point = { x: dst_cx, y: dst_cy }
    let mid : @types.Point = { x: dst_cx, y: src_cy }
    grid.carve_line(from, mid, @types.Corridor)
    grid.carve_line(mid, to, @types.Corridor)
    // Place LockedDoor at the bend
    if grid.in_bounds(mid.x, mid.y) {
      grid.set(mid.x, mid.y, @types.LockedDoor)
    }
  }
  // Place Start in first biome, Goal in last biome
  if nodes.length() > 0 {
    let first_pos = positions[0]
    let start_x = first_pos.0 + first_pos.2 / 2
    let start_y = first_pos.1 + first_pos.3 / 2
    if grid.in_bounds(start_x, start_y) {
      grid.set(start_x, start_y, @types.Start)
    }
    // Find the last-column node(s) and pick one for the goal
    let mut goal_idx = nodes.length() - 1
    let mut max_col = 0
    for i, node in nodes {
      if node.col > max_col {
        max_col = node.col
        goal_idx = i
      }
    }
    let last_pos = positions[goal_idx]
    let goal_x = last_pos.0 + last_pos.2 / 2
    let goal_y = last_pos.1 + last_pos.3 / 2
    if grid.in_bounds(goal_x, goal_y) {
      grid.set(goal_x, goal_y, @types.Goal)
    }
  }
  (grid, rooms)
}

///|
pub fn generate(
  config : DeadCellsConfig,
  rng : @types.RNG
) -> DeadCellsResult {
  let nodes = build_biome_graph(
    config.path_length, config.max_branches, config.branch_chance, rng,
  )
  assign_biome_sizes(nodes, config.biome_min_size, config.biome_max_size, rng)
  let shortcuts = add_shortcuts(nodes, config.shortcut_chance, rng)
  let (grid, rooms) = render_dead_cells_grid(nodes, shortcuts, config)
  { grid, rooms }
}
