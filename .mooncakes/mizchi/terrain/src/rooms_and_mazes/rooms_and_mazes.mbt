///|
pub(all) struct RoomsAndMazesConfig {
  width : Int
  height : Int
  num_room_attempts : Int
  room_min_size : Int
  room_max_size : Int
  winding_chance : Double
  extra_connector_chance : Double
}

///|
pub fn RoomsAndMazesConfig::default() -> RoomsAndMazesConfig {
  {
    width: 41,
    height: 21,
    num_room_attempts: 50,
    room_min_size: 3,
    room_max_size: 7,
    winding_chance: 0.3,
    extra_connector_chance: 0.02,
  }
}

///|
pub(all) struct RoomsAndMazesResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(
  config : RoomsAndMazesConfig,
  rng : @types.RNG
) -> RoomsAndMazesResult {
  // Ensure odd dimensions
  let w = if config.width % 2 == 0 { config.width - 1 } else { config.width }
  let h = if config.height % 2 == 0 { config.height - 1 } else { config.height }
  let grid = @types.Grid2D::make(w, h, @types.Wall)
  // Phase 1: Place non-overlapping rooms (odd-aligned)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  for _ in 0..<config.num_room_attempts {
    let rw = rng.rand_range(config.room_min_size / 2, config.room_max_size / 2 + 1) *
      2 +
      1
    let rh = rng.rand_range(config.room_min_size / 2, config.room_max_size / 2 + 1) *
      2 +
      1
    let rx = rng.rand_range(0, (w - rw) / 2) * 2 + 1
    let ry = rng.rand_range(0, (h - rh) / 2) * 2 + 1
    if rx + rw >= w || ry + rh >= h {
      continue
    }
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
  // Phase 2: Fill remaining space with maze (growing tree)
  // Region tracking: -1=wall, room_id for rooms, unique ids for maze regions
  let regions = Array::make(w * h, -1)
  let mut current_region = 0
  // Assign regions to rooms
  for room in rooms {
    for y in room.rect.y..<(room.rect.y + room.rect.h) {
      for x in room.rect.x..<(room.rect.x + room.rect.w) {
        regions[y * w + x] = current_region
      }
    }
    current_region += 1
  }
  // Grow maze from each uncarved odd cell
  for y_ in 0..<((h - 1) / 2) {
    let y = y_ * 2 + 1
    for x_ in 0..<((w - 1) / 2) {
      let x = x_ * 2 + 1
      if grid.get(x, y) != @types.Wall {
        continue
      }
      grow_maze(grid, regions, x, y, w, h, current_region, config.winding_chance, rng)
      current_region += 1
    }
  }
  // Phase 3: Connect regions via connectors
  connect_regions(grid, regions, w, h, current_region, config.extra_connector_chance, rng)
  // Phase 4: Remove dead ends
  remove_dead_ends(grid, w, h)
  // Place Start and Goal
  if rooms.length() >= 2 {
    let sc = rooms[0].rect.center()
    grid.set(sc.x, sc.y, @types.Start)
    let gc = rooms[rooms.length() - 1].rect.center()
    grid.set(gc.x, gc.y, @types.Goal)
  }
  { grid, rooms }
}

///|
fn grow_maze(
  grid : @types.Grid2D,
  regions : Array[Int],
  start_x : Int,
  start_y : Int,
  w : Int,
  h : Int,
  region : Int,
  winding_chance : Double,
  rng : @types.RNG
) -> Unit {
  let cells : Array[@types.Point] = []
  grid.set(start_x, start_y, @types.Corridor)
  regions[start_y * w + start_x] = region
  cells.push({ x: start_x, y: start_y })
  let mut last_dir : (Int, Int) = (0, 0)
  while cells.length() > 0 {
    let cell = cells[cells.length() - 1]
    let unmade = get_carvable_dirs(grid, cell.x, cell.y, w, h)
    if unmade.length() == 0 {
      ignore(cells.pop())
      last_dir = (0, 0)
    } else {
      // Prefer last direction for winding
      let dir = if last_dir.0 != 0 || last_dir.1 != 0 {
        let mut has_last = false
        for d in unmade {
          if d.0 == last_dir.0 && d.1 == last_dir.1 {
            has_last = true
            break
          }
        }
        if has_last && rng.rand_bool(winding_chance) {
          last_dir
        } else {
          rng.rand_pick(unmade)
        }
      } else {
        rng.rand_pick(unmade)
      }
      // Carve wall and destination
      let wx = cell.x + dir.0
      let wy = cell.y + dir.1
      let nx = cell.x + dir.0 * 2
      let ny = cell.y + dir.1 * 2
      grid.set(wx, wy, @types.Corridor)
      grid.set(nx, ny, @types.Corridor)
      regions[wy * w + wx] = region
      regions[ny * w + nx] = region
      cells.push({ x: nx, y: ny })
      last_dir = dir
    }
  }
}

///|
fn get_carvable_dirs(
  grid : @types.Grid2D,
  x : Int,
  y : Int,
  w : Int,
  h : Int
) -> Array[(Int, Int)] {
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  let result : Array[(Int, Int)] = []
  for d in dirs {
    let nx = x + d.0 * 2
    let ny = y + d.1 * 2
    if nx > 0 && nx < w - 1 && ny > 0 && ny < h - 1 &&
      grid.get(nx, ny) == @types.Wall {
      result.push(d)
    }
  }
  result
}

///|
fn connect_regions(
  grid : @types.Grid2D,
  regions : Array[Int],
  w : Int,
  h : Int,
  num_regions : Int,
  extra_chance : Double,
  rng : @types.RNG
) -> Unit {
  // Find all connectors (wall cells adjacent to two different regions)
  let connectors : Array[@types.Point] = []
  for y in 1..<(h - 1) {
    for x in 1..<(w - 1) {
      if grid.get(x, y) != @types.Wall {
        continue
      }
      let adj_regions = get_adjacent_regions(regions, x, y, w)
      if adj_regions.length() >= 2 {
        connectors.push({ x, y })
      }
    }
  }
  if connectors.length() == 0 {
    return
  }
  // Union-Find for merging regions
  let parent = Array::makei(num_regions, fn(i) { i })
  // Shuffle connectors
  rng.rand_shuffle(connectors)
  for conn in connectors {
    let adj = get_adjacent_regions(regions, conn.x, conn.y, w)
    if adj.length() < 2 {
      continue
    }
    let r1 = find_root(parent, adj[0])
    let r2 = find_root(parent, adj[1])
    if r1 != r2 {
      // Must connect
      grid.set(conn.x, conn.y, @types.Door)
      union(parent, r1, r2)
    } else if rng.rand_bool(extra_chance) {
      grid.set(conn.x, conn.y, @types.Door)
    }
  }
}

///|
fn get_adjacent_regions(
  regions : Array[Int],
  x : Int,
  y : Int,
  w : Int
) -> Array[Int] {
  let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
  let result : Array[Int] = []
  for d in dirs {
    let nx = x + d.0
    let ny = y + d.1
    let r = regions[ny * w + nx]
    if r >= 0 {
      let mut found = false
      for existing in result {
        if existing == r {
          found = true
          break
        }
      }
      if not(found) {
        result.push(r)
      }
    }
  }
  result
}

///|
fn find_root(parent : Array[Int], x : Int) -> Int {
  let mut current = x
  while parent[current] != current {
    parent[current] = parent[parent[current]]
    current = parent[current]
  }
  current
}

///|
fn union(parent : Array[Int], a : Int, b : Int) -> Unit {
  let ra = find_root(parent, a)
  let rb = find_root(parent, b)
  if ra != rb {
    parent[ra] = rb
  }
}

///|
fn remove_dead_ends(grid : @types.Grid2D, w : Int, h : Int) -> Unit {
  let mut changed = true
  while changed {
    changed = false
    for y in 1..<(h - 1) {
      for x in 1..<(w - 1) {
        let tile = grid.get(x, y)
        if tile != @types.Corridor {
          continue
        }
        // Count walkable neighbors
        let mut exits = 0
        let dirs : Array[(Int, Int)] = [(0, -1), (0, 1), (-1, 0), (1, 0)]
        for d in dirs {
          let t = grid.get(x + d.0, y + d.1)
          if t != @types.Wall {
            exits += 1
          }
        }
        if exits <= 1 {
          grid.set(x, y, @types.Wall)
          changed = true
        }
      }
    }
  }
}
