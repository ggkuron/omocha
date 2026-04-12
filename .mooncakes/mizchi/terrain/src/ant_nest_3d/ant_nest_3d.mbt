///|
pub(all) struct AntNest3dConfig {
  width : Int
  height : Int
  depth : Int
  num_entrances : Int
  chamber_chance : Double
}

///|
pub fn AntNest3dConfig::default() -> AntNest3dConfig {
  { width: 40, height: 20, depth: 4, num_entrances: 3, chamber_chance: 0.3 }
}

///|
pub(all) struct AntNest3dResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
fn carve_chamber(
  grid : @types.Grid3D,
  cx : Int,
  cy : Int,
  z : Int,
  tile : @types.Tile,
  rng : @types.RNG,
  rooms : Array[@types.Room],
  room_id_counter : Array[Int]
) -> Unit {
  let w = rng.rand_range(3, 6)
  let h = rng.rand_range(3, 6)
  let rx = cx - w / 2
  let ry = cy - h / 2
  if rx < 0 || ry < 0 || rx + w >= grid.width || ry + h >= grid.height {
    return
  }
  for dy in 0..<h {
    for dx in 0..<w {
      grid.set(rx + dx, ry + dy, z, @types.Floor)
    }
  }
  // Place special tile at center
  grid.set(cx, cy, z, tile)
  let rid = room_id_counter[0]
  room_id_counter[0] = rid + 1
  rooms.push({ rect: { x: rx, y: ry, w, h }, id: rid })
}

///|
fn dig_tunnel(
  grid : @types.Grid3D,
  x1 : Int,
  y1 : Int,
  x2 : Int,
  y2 : Int,
  z : Int,
  rng : @types.RNG
) -> Unit {
  let mut x = x1
  let mut y = y1
  let max_steps = grid.width + grid.height
  let mut steps = 0
  while (x != x2 || y != y2) && steps < max_steps {
    steps += 1
    if x >= 0 && x < grid.width && y >= 0 && y < grid.height {
      grid.set(x, y, z, @types.Corridor)
    }
    // Move towards target with organic wobble
    if rng.rand_bool(0.7) {
      // Move towards target
      if x != x2 {
        if x < x2 {
          x += 1
        } else {
          x -= 1
        }
      } else if y != y2 {
        if y < y2 {
          y += 1
        } else {
          y -= 1
        }
      }
    } else {
      // Random wobble
      let dir = rng.rand_direction()
      let nx = x + dir.x
      let ny = y + dir.y
      if nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height {
        x = nx
        y = ny
      }
    }
  }
  if x >= 0 && x < grid.width && y >= 0 && y < grid.height {
    grid.set(x, y, z, @types.Corridor)
  }
}

///|
pub fn generate(
  config : AntNest3dConfig,
  rng : @types.RNG
) -> AntNest3dResult {
  let width = config.width
  let height = config.height
  let depth_count = config.depth
  let grid = @types.Grid3D::make(width, height, depth_count, @types.Wall)
  let rooms : Array[@types.Room] = []
  let room_id_counter : Array[Int] = [0]
  // Layer 0: Surface with entrance holes
  for y in 0..<height {
    for x in 0..<width {
      grid.set(x, y, 0, @types.Floor)
    }
  }
  // Place entrances on surface
  let entrances : Array[(Int, Int)] = []
  for _i in 0..<config.num_entrances {
    let ex = rng.rand_range(3, width - 3)
    let ey = rng.rand_range(3, height - 3)
    entrances.push((ex, ey))
    grid.set(ex, ey, 0, @types.Door)
    // Mark entrance on surface
    let rid = room_id_counter[0]
    room_id_counter[0] = rid + 1
    rooms.push({ rect: { x: ex - 1, y: ey - 1, w: 3, h: 3 }, id: rid })
  }
  // For each entrance, dig tunnels downward
  for entrance in entrances {
    let mut cur_x = entrance.0
    let mut cur_y = entrance.1
    for z in 1..<depth_count {
      // Vertical shaft down
      grid.set(cur_x, cur_y, z, @types.Corridor)
      // Dig branching tunnels on this layer
      let num_branches = rng.rand_range(2, 5)
      for _b in 0..<num_branches {
        let target_x = rng.rand_range(2, width - 2)
        let target_y = rng.rand_range(2, height - 2)
        dig_tunnel(grid, cur_x, cur_y, target_x, target_y, z, rng)
        // Maybe place a chamber at tunnel end
        if rng.rand_bool(config.chamber_chance) {
          let chamber_tile = if z == depth_count - 1 {
            @types.Start // queen chamber on deepest
          } else if rng.rand_bool(0.5) {
            @types.Key // food storage
          } else {
            @types.Goal // nursery
          }
          carve_chamber(
            grid, target_x, target_y, z, chamber_tile, rng, rooms,
            room_id_counter,
          )
        }
      }
      // Wander to a new position for next layer's shaft
      cur_x += rng.rand_range(-3, 4)
      cur_y += rng.rand_range(-3, 4)
      if cur_x < 2 {
        cur_x = 2
      }
      if cur_x >= width - 2 {
        cur_x = width - 3
      }
      if cur_y < 2 {
        cur_y = 2
      }
      if cur_y >= height - 2 {
        cur_y = height - 3
      }
    }
  }
  // Ensure queen chamber exists on deepest layer if not already placed
  let deepest = depth_count - 1
  let mut has_queen = false
  for y in 0..<height {
    for x in 0..<width {
      if grid.get(x, y, deepest) == @types.Start {
        has_queen = true
        break
      }
    }
    if has_queen {
      break
    }
  }
  if not(has_queen) && depth_count > 1 {
    let qx = width / 2
    let qy = height / 2
    carve_chamber(
      grid, qx, qy, deepest, @types.Start, rng, rooms, room_id_counter,
    )
    // Connect queen chamber to nearest entrance shaft
    if entrances.length() > 0 {
      let e = entrances[0]
      dig_tunnel(grid, e.0, e.1, qx, qy, deepest, rng)
    }
  }
  { grid, rooms }
}
