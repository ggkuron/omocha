///|
pub(all) struct Maze3dConfig {
  width : Int
  height : Int
  depth : Int
  up_chance : Double
}

///|
pub fn Maze3dConfig::default() -> Maze3dConfig {
  { width: 21, height: 21, depth: 3, up_chance: 0.15 }
}

///|
pub(all) struct Maze3dResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
priv struct Cell3D {
  x : Int
  y : Int
  z : Int
}

///|
fn is_visited(
  grid : @types.Grid3D,
  x : Int,
  y : Int,
  z : Int
) -> Bool {
  grid.get(x, y, z) != @types.Wall
}

///|
fn get_unvisited_neighbors_3d(
  grid : @types.Grid3D,
  cx : Int,
  cy : Int,
  cz : Int,
  w : Int,
  h : Int,
  d : Int,
  rng : @types.RNG,
  up_chance : Double
) -> Array[Cell3D] {
  let result : Array[Cell3D] = []
  // 4 horizontal directions (step by 2 for maze cells)
  let dirs : Array[(Int, Int)] = [(0, -2), (0, 2), (-2, 0), (2, 0)]
  for dir in dirs {
    let nx = cx + dir.0
    let ny = cy + dir.1
    if nx > 0 && nx < w - 1 && ny > 0 && ny < h - 1 {
      if not(is_visited(grid, nx, ny, cz)) {
        result.push({ x: nx, y: ny, z: cz })
      }
    }
  }
  // Vertical directions (up/down) with probability gate
  if rng.rand_bool(up_chance) {
    if cz + 1 < d && not(is_visited(grid, cx, cy, cz + 1)) {
      result.push({ x: cx, y: cy, z: cz + 1 })
    }
  }
  if rng.rand_bool(up_chance) {
    if cz - 1 >= 0 && not(is_visited(grid, cx, cy, cz - 1)) {
      result.push({ x: cx, y: cy, z: cz - 1 })
    }
  }
  result
}

///|
pub fn generate(config : Maze3dConfig, rng : @types.RNG) -> Maze3dResult {
  // Ensure odd dimensions
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 {
    config.height - 1
  } else {
    config.height
  }
  let d = if config.depth < 1 { 1 } else { config.depth }
  let grid = @types.Grid3D::make(w, h, d, @types.Wall)
  // Start DFS from (1,1,0)
  let start_x = 1
  let start_y = 1
  let start_z = 0
  grid.set(start_x, start_y, start_z, @types.Corridor)
  let stack : Array[Cell3D] = [{ x: start_x, y: start_y, z: start_z }]
  while stack.length() > 0 {
    let current = stack[stack.length() - 1]
    let neighbors = get_unvisited_neighbors_3d(
      grid, current.x, current.y, current.z, w, h, d, rng, config.up_chance,
    )
    if neighbors.length() == 0 {
      ignore(stack.pop())
    } else {
      let next = rng.rand_pick(neighbors)
      if next.z == current.z {
        // Horizontal move: carve wall between current and next
        let wall_x = current.x + (next.x - current.x) / 2
        let wall_y = current.y + (next.y - current.y) / 2
        grid.set(wall_x, wall_y, current.z, @types.Corridor)
        grid.set(next.x, next.y, next.z, @types.Corridor)
      } else {
        // Vertical move: mark both cells and use Door as stair marker
        grid.set(current.x, current.y, current.z, @types.Door)
        grid.set(next.x, next.y, next.z, @types.Door)
      }
      stack.push(next)
    }
  }
  // Collect "rooms" as cells at odd coordinates that became corridors
  // We treat each carved cell as a 1x1 room for the result
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for z in 0..<d {
    for y = 1; y < h - 1; y = y + 2 {
      for x = 1; x < w - 1; x = x + 2 {
        let tile = grid.get(x, y, z)
        if tile == @types.Corridor || tile == @types.Door {
          let rect : @types.Rect = { x, y, w: 1, h: 1 }
          rooms.push({ rect, id: room_id })
          room_id += 1
        }
      }
    }
  }
  // Place Start and Goal
  grid.set(1, 1, 0, @types.Start)
  grid.set(w - 2, h - 2, d - 1, @types.Goal)
  { grid, rooms }
}
