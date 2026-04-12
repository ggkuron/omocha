///|
pub(all) struct GraphGrammarConfig {
  width : Int
  height : Int
  num_rooms : Int
  num_locks : Int
  room_size : Int
}

///|
pub fn GraphGrammarConfig::default() -> GraphGrammarConfig {
  { width: 40, height: 20, num_rooms: 6, num_locks: 2, room_size: 4 }
}

///|
pub(all) struct GraphGrammarResult {
  grid : @types.Grid2D
  graph : Array[@types.GraphNode]
  rooms : Array[@types.Room]
}

///|
fn build_mission_graph(
  num_rooms : Int,
  num_locks : Int,
  rng : @types.RNG
) -> Array[@types.GraphNode] {
  let nodes : Array[@types.GraphNode] = []
  // Start node
  nodes.push({ id: 0, kind: @types.StartNode, children: [] })
  // Room nodes
  for i in 1..<num_rooms {
    nodes.push({ id: i, kind: @types.RoomNode, children: [] })
  }
  // Goal node
  let goal_id = num_rooms
  nodes.push({ id: goal_id, kind: @types.GoalNode, children: [] })
  // Linear chain with branches
  for i in 0..<num_rooms {
    let next = if i < num_rooms - 1 { i + 1 } else { goal_id }
    nodes[i].children.push(next)
  }
  // Insert lock-key pairs
  let mut locks_placed = 0
  for i in 1..<num_rooms {
    if locks_placed >= num_locks {
      break
    }
    if rng.rand_bool(0.5) && i > 1 {
      // Turn this node into a lock
      nodes[i] = { ..nodes[i], kind: @types.LockNode }
      // Add key as branch from an earlier node
      let key_id = nodes.length()
      let key_parent = rng.rand_range(0, i)
      nodes.push({ id: key_id, kind: @types.KeyNode, children: [] })
      nodes[key_parent].children.push(key_id)
      locks_placed += 1
    }
  }
  nodes
}

///|
fn layout_rooms(
  graph : Array[@types.GraphNode],
  width : Int,
  height : Int,
  room_size : Int,
  rng : @types.RNG
) -> Array[@types.Room] {
  let rooms : Array[@types.Room] = []
  let margin = 2
  let usable_w = width - margin * 2 - room_size
  let usable_h = height - margin * 2 - room_size
  // Place rooms at pseudo-random positions avoiding overlap
  for i in 0..<graph.length() {
    ignore(i)
    for _ in 0..<50 {
      let x = rng.rand_range(margin, margin + usable_w)
      let y = rng.rand_range(margin, margin + usable_h)
      let rect : @types.Rect = { x, y, w: room_size, h: room_size }
      let mut overlaps = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        rooms.push({ rect, id: rooms.length() })
        break
      }
    }
  }
  rooms
}

///|
pub fn generate(
  config : GraphGrammarConfig,
  rng : @types.RNG
) -> GraphGrammarResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let graph = build_mission_graph(config.num_rooms, config.num_locks, rng)
  let rooms = layout_rooms(graph, config.width, config.height, config.room_size, rng)
  // Carve rooms and place special tiles
  for i, room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
    if i < graph.length() {
      let center = room.rect.center()
      match graph[i].kind {
        @types.StartNode => grid.set(center.x, center.y, @types.Start)
        @types.GoalNode => grid.set(center.x, center.y, @types.Goal)
        @types.KeyNode => grid.set(center.x, center.y, @types.Key)
        _ => ()
      }
    }
  }
  // Connect rooms according to graph edges
  for node in graph {
    if node.id < rooms.length() {
      let from_center = rooms[node.id].rect.center()
      for child_id in node.children {
        if child_id < rooms.length() {
          let to_center = rooms[child_id].rect.center()
          // Check if this connection should be a locked door
          let tile = if child_id < graph.length() && graph[child_id].kind == @types.LockNode {
            @types.LockedDoor
          } else {
            @types.Corridor
          }
          carve_corridor_with_door(grid, from_center, to_center, tile)
        }
      }
    }
  }
  { grid, graph, rooms }
}

///|
fn carve_corridor_with_door(
  grid : @types.Grid2D,
  from : @types.Point,
  to : @types.Point,
  door_tile : @types.Tile
) -> Unit {
  let mid : @types.Point = { x: to.x, y: from.y }
  grid.carve_line(from, mid, @types.Corridor)
  grid.carve_line(mid, to, @types.Corridor)
  // Place door tile at connection point
  if door_tile == @types.LockedDoor {
    grid.set(mid.x, mid.y, door_tile)
  }
}
