///|
pub(all) struct OnewayGateConfig {
  width : Int
  height : Int
  num_rooms : Int
  branch_factor : Int
}

///|
pub fn OnewayGateConfig::default() -> OnewayGateConfig {
  { width: 50, height: 30, num_rooms: 10, branch_factor: 2 }
}

///|
pub(all) struct OnewayGateResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Node in the directed tree structure
priv struct TreeNode {
  id : Int
  children : Array[Int]
}

///| Build a tree by assigning children to each node breadth-first
fn build_tree(num_rooms : Int, branch_factor : Int, rng : @types.RNG) -> Array[TreeNode] {
  let nodes : Array[TreeNode] = []
  for i in 0..<num_rooms {
    nodes.push({ id: i, children: [] })
  }
  // BFS assignment: each node gets up to branch_factor children
  let queue : Array[Int] = [0]
  let mut next_id = 1
  let mut qi = 0
  while qi < queue.length() && next_id < num_rooms {
    let current = queue[qi]
    qi += 1
    // Vary the number of children (1 to branch_factor)
    let num_children = rng.rand_range(1, branch_factor + 1)
    for _ in 0..<num_children {
      if next_id >= num_rooms {
        break
      }
      nodes[current].children.push(next_id)
      queue.push(next_id)
      next_id += 1
    }
  }
  nodes
}

///| Check if a node is a dead-end (leaf node)
fn is_leaf(nodes : Array[TreeNode], id : Int) -> Bool {
  nodes[id].children.length() == 0
}

///| Place rooms on the grid avoiding overlap
fn place_rooms_on_grid(
  nodes : Array[TreeNode],
  config : OnewayGateConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let min_size = 4
  let max_size = 7
  for _node in nodes {
    let rw = rng.rand_range(min_size, max_size + 1)
    let rh = rng.rand_range(min_size, max_size + 1)
    let mut placed = false
    for _ in 0..<80 {
      let rx = rng.rand_range(1, config.width - rw - 1)
      let ry = rng.rand_range(1, config.height - rh - 1)
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
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
      let rx = rng.rand_range(1, config.width - rw - 1)
      let ry = rng.rand_range(1, config.height - rh - 1)
      rooms.push({ rect: { x: rx, y: ry, w: rw, h: rh }, id: rooms.length() })
    }
  }
  rooms
}

///| Carve corridor with a Door tile at the midpoint (one-way gate)
fn carve_oneway_corridor(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point
) -> Unit {
  let mid : @types.Point = { x: to.x, y: from.y }
  grid.carve_line(from, mid, @types.Corridor)
  grid.carve_line(mid, to, @types.Corridor)
  // Place door (one-way gate) at the bend
  if grid.in_bounds(mid.x, mid.y) {
    grid.set(mid.x, mid.y, @types.Door)
  }
}

///|
pub fn generate(config : OnewayGateConfig, rng : @types.RNG) -> OnewayGateResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let nodes = build_tree(config.num_rooms, config.branch_factor, rng)
  let rooms = place_rooms_on_grid(nodes, config, rng)
  // Carve all rooms
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect parent->child with one-way gate corridors
  for node in nodes {
    for child_id in node.children {
      if node.id < rooms.length() && child_id < rooms.length() {
        let from_center = rooms[node.id].rect.center()
        let to_center = rooms[child_id].rect.center()
        carve_oneway_corridor(grid, from_center, to_center)
      }
    }
  }
  // Place Start at root room
  if rooms.length() > 0 {
    let start_center = rooms[0].rect.center()
    grid.set(start_center.x, start_center.y, @types.Start)
  }
  // Place Key rewards at dead-end rooms
  for i in 0..<nodes.length() {
    if i < rooms.length() && is_leaf(nodes, i) && i != 0 {
      let center = rooms[i].rect.center()
      grid.set(center.x, center.y, @types.Key)
    }
  }
  // Place Goal at the deepest leaf (last room in BFS order that is a leaf)
  for i = nodes.length() - 1; i >= 1; i = i - 1 {
    if i < rooms.length() && is_leaf(nodes, i) {
      let center = rooms[i].rect.center()
      grid.set(center.x, center.y, @types.Goal)
      break
    }
  }
  { grid, rooms }
}
