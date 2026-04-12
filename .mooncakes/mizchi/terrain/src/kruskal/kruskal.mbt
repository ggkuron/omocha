///|
pub(all) struct KruskalConfig {
  width : Int
  height : Int
  room_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn KruskalConfig::default() -> KruskalConfig {
  {
    width: 41,
    height: 21,
    room_chance: 0.12,
    room_min_size: 3,
    room_max_size: 5,
  }
}

///|
pub(all) struct KruskalResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : KruskalConfig, rng : @types.RNG) -> KruskalResult {
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let n = maze_w * maze_h
  // Carve all cells
  for row in 0..<maze_h {
    for col in 0..<maze_w {
      grid.set(col * 2 + 1, row * 2 + 1, @types.Corridor)
    }
  }
  // Union-Find
  let parent = Array::makei(n, fn(i) { i })
  let rank = Array::make(n, 0)
  // Collect all edges (walls between adjacent cells)
  let wall_edges : Array[(Int, Int, Int, Int)] = []
  for row in 0..<maze_h {
    for col in 0..<maze_w {
      let cell = row * maze_w + col
      // Right neighbor
      if col + 1 < maze_w {
        let right = row * maze_w + (col + 1)
        let wx = col * 2 + 2
        let wy = row * 2 + 1
        wall_edges.push((cell, right, wx, wy))
      }
      // Down neighbor
      if row + 1 < maze_h {
        let down = (row + 1) * maze_w + col
        let wx = col * 2 + 1
        let wy = row * 2 + 2
        wall_edges.push((cell, down, wx, wy))
      }
    }
  }
  // Shuffle edges
  rng.rand_shuffle(wall_edges)
  // Kruskal: remove walls if cells are in different sets
  for e in wall_edges {
    let a = find(parent, e.0)
    let b = find(parent, e.1)
    if a != b {
      union(parent, rank, a, b)
      grid.set(e.2, e.3, @types.Corridor)
    }
  }
  // Place rooms
  let rooms = place_rooms(grid, w, h, config, rng)
  // Start and Goal
  grid.set(1, 1, @types.Start)
  grid.set(w - 2, h - 2, @types.Goal)
  { grid, rooms }
}

///|
fn find(parent : Array[Int], x : Int) -> Int {
  let mut i = x
  while parent[i] != i {
    parent[i] = parent[parent[i]]
    i = parent[i]
  }
  i
}

///|
fn union(parent : Array[Int], rank : Array[Int], a : Int, b : Int) -> Unit {
  if rank[a] < rank[b] {
    parent[a] = b
  } else if rank[a] > rank[b] {
    parent[b] = a
  } else {
    parent[b] = a
    rank[a] += 1
  }
}

///|
fn place_rooms(
  grid : @types.Grid2D,
  w : Int,
  h : Int,
  config : KruskalConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let maze_w = (w - 1) / 2
  let maze_h = (h - 1) / 2
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for _ in 0..<(maze_w * maze_h) {
    if not(rng.rand_bool(config.room_chance)) {
      continue
    }
    let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
    let rx = rng.rand_range(1, w - rw - 1)
    let ry = rng.rand_range(1, h - rh - 1)
    if rx + rw < w && ry + rh < h {
      let rect : @types.Rect = { x: rx, y: ry, w: rw, h: rh }
      let mut overlaps = false
      for r in rooms {
        let padded : @types.Rect = {
          x: r.rect.x - 1,
          y: r.rect.y - 1,
          w: r.rect.w + 2,
          h: r.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        grid.fill_rect(rect, @types.Floor)
        rooms.push({ rect, id: room_id })
        room_id += 1
      }
    }
  }
  rooms
}
