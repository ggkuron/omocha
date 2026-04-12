///|
pub(all) struct PrefabConfig {
  width : Int
  height : Int
  num_prefabs : Int
  max_attempts : Int
}

///|
pub fn PrefabConfig::default() -> PrefabConfig {
  { width: 50, height: 25, num_prefabs: 12, max_attempts: 100 }
}

///|
pub(all) struct PrefabResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : PrefabConfig, rng : @types.RNG) -> PrefabResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let mut room_id = 0
  // Place first prefab near center
  let first = get_prefab(rng)
  let cx = config.width / 2 - first.w / 2
  let cy = config.height / 2 - first.h / 2
  stamp_prefab(grid, first, cx, cy)
  rooms.push({
    rect: { x: cx, y: cy, w: first.w, h: first.h },
    id: room_id,
  })
  room_id += 1
  // Place remaining prefabs by connecting to existing rooms
  for _ in 1..<config.num_prefabs {
    let prefab = get_prefab(rng)
    let mut placed = false
    for _ in 0..<config.max_attempts {
      // Pick a random existing room
      let target = rooms[rng.next_int(rooms.length())]
      // Pick a random side (0=right, 1=bottom, 2=left, 3=top)
      let side = rng.next_int(4)
      let pos = match side {
        0 =>
          (
            target.rect.x + target.rect.w + 1,
            target.rect.y + rng.next_int(target.rect.h.max(1)) -
              rng.next_int(prefab.h.max(1)),
          )
        1 =>
          (
            target.rect.x + rng.next_int(target.rect.w.max(1)) -
              rng.next_int(prefab.w.max(1)),
            target.rect.y + target.rect.h + 1,
          )
        2 =>
          (
            target.rect.x - prefab.w - 1,
            target.rect.y + rng.next_int(target.rect.h.max(1)) -
              rng.next_int(prefab.h.max(1)),
          )
        _ =>
          (
            target.rect.x + rng.next_int(target.rect.w.max(1)) -
              rng.next_int(prefab.w.max(1)),
            target.rect.y - prefab.h - 1,
          )
      }
      let px = pos.0
      let py = pos.1
      if px > 0 &&
        py > 0 &&
        px + prefab.w < config.width - 1 &&
        py + prefab.h < config.height - 1 {
        // Check no overlap with existing rooms
        let new_rect : @types.Rect = { x: px, y: py, w: prefab.w, h: prefab.h }
        let mut overlaps = false
        for r in rooms {
          let padded : @types.Rect = {
            x: r.rect.x - 1,
            y: r.rect.y - 1,
            w: r.rect.w + 2,
            h: r.rect.h + 2,
          }
          if new_rect.intersects(padded) {
            overlaps = true
            break
          }
        }
        if not(overlaps) {
          stamp_prefab(grid, prefab, px, py)
          rooms.push({ rect: new_rect, id: room_id })
          room_id += 1
          // Connect to target room via corridor
          let from = target.rect.center()
          let to : @types.Point = {
            x: px + prefab.w / 2,
            y: py + prefab.h / 2,
          }
          grid.carve_l_corridor(from, to, @types.Corridor)
          placed = true
          break
        }
      }
    }
    if not(placed) {
      continue
    }
  }
  // Ensure connectivity
  ensure_connectivity(grid, config)
  // Place Start and Goal
  if rooms.length() > 0 {
    let sc = rooms[0].rect.center()
    grid.set(sc.x, sc.y, @types.Start)
  }
  if rooms.length() > 1 {
    let gc = rooms[rooms.length() - 1].rect.center()
    grid.set(gc.x, gc.y, @types.Goal)
  }
  { grid, rooms }
}

///|
priv struct Prefab {
  w : Int
  h : Int
  pattern : Array[Array[@types.Tile]]
}

///|
fn get_prefab(rng : @types.RNG) -> Prefab {
  let kind = rng.next_int(6)
  match kind {
    0 =>
      // Small square room (3x3)
      {
        w: 3,
        h: 3,
        pattern: [
          [@types.Floor, @types.Floor, @types.Floor],
          [@types.Floor, @types.Floor, @types.Floor],
          [@types.Floor, @types.Floor, @types.Floor],
        ],
      }
    1 =>
      // T-junction (5x3)
      {
        w: 5,
        h: 3,
        pattern: [
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Wall, @types.Wall, @types.Floor, @types.Wall, @types.Wall,
          ],
          [
            @types.Wall, @types.Wall, @types.Floor, @types.Wall, @types.Wall,
          ],
        ],
      }
    2 =>
      // Cross (5x5)
      {
        w: 5,
        h: 5,
        pattern: [
          [
            @types.Wall, @types.Wall, @types.Floor, @types.Wall, @types.Wall,
          ],
          [
            @types.Wall, @types.Floor, @types.Floor, @types.Floor, @types.Wall,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Wall, @types.Floor, @types.Floor, @types.Floor, @types.Wall,
          ],
          [
            @types.Wall, @types.Wall, @types.Floor, @types.Wall, @types.Wall,
          ],
        ],
      }
    3 =>
      // L-shape (4x4)
      {
        w: 4,
        h: 4,
        pattern: [
          [
            @types.Floor, @types.Floor, @types.Wall, @types.Wall,
          ],
          [
            @types.Floor, @types.Floor, @types.Wall, @types.Wall,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
        ],
      }
    4 =>
      // Hallway horizontal (6x2)
      {
        w: 6,
        h: 2,
        pattern: [
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor,
            @types.Floor, @types.Floor,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor,
            @types.Floor, @types.Floor,
          ],
        ],
      }
    _ =>
      // Large room (5x4)
      {
        w: 5,
        h: 4,
        pattern: [
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
          [
            @types.Floor, @types.Floor, @types.Floor, @types.Floor, @types.Floor,
          ],
        ],
      }
  }
}

///|
fn stamp_prefab(
  grid : @types.Grid2D,
  prefab : Prefab,
  ox : Int,
  oy : Int
) -> Unit {
  for y in 0..<prefab.h {
    for x in 0..<prefab.w {
      let gx = ox + x
      let gy = oy + y
      if grid.in_bounds(gx, gy) {
        let tile = prefab.pattern[y][x]
        if tile != @types.Wall {
          grid.set(gx, gy, tile)
        }
      }
    }
  }
}

///|
fn ensure_connectivity(
  grid : @types.Grid2D,
  config : PrefabConfig
) -> Unit {
  let visited = Array::make(config.width * config.height, false)
  let components : Array[Array[@types.Point]] = []
  for y in 1..<(config.height - 1) {
    for x in 1..<(config.width - 1) {
      let idx = y * config.width + x
      let t = grid.get(x, y)
      if not(visited[idx]) &&
        (t == @types.Floor || t == @types.Corridor || t == @types.Door || t == @types.Start ||
        t == @types.Goal) {
        let comp = @types.flood_fill(grid, x, y)
        for p in comp {
          visited[p.y * config.width + p.x] = true
        }
        components.push(comp)
      }
    }
  }
  if components.length() <= 1 {
    return
  }
  let mut largest_idx = 0
  let mut largest_size = 0
  for i in 0..<components.length() {
    if components[i].length() > largest_size {
      largest_size = components[i].length()
      largest_idx = i
    }
  }
  for i in 0..<components.length() {
    if i == largest_idx {
      continue
    }
    let a = components[i][0]
    let b = components[largest_idx][0]
    grid.carve_l_corridor(a, b, @types.Corridor)
  }
}
