///|
pub(all) struct TreeRootsConfig {
  width : Int
  height : Int
  depth : Int
  branch_chance : Double
}

///|
pub fn TreeRootsConfig::default() -> TreeRootsConfig {
  { width: 40, height: 20, depth: 4, branch_chance: 0.4 }
}

///|
pub(all) struct TreeRootsResult {
  grid : @types.Grid3D
  rooms : Array[@types.Room]
}

///|
fn grow_root(
  grid : @types.Grid3D,
  x : Int,
  y : Int,
  z : Int,
  dx : Int,
  dy : Int,
  rng : @types.RNG,
  branch_chance : Double,
  rooms : Array[@types.Room],
  room_id_counter : Array[Int],
  max_steps : Int
) -> Unit {
  let mut cx = x
  let mut cy = y
  let mut cz = z
  let mut cur_dx = dx
  let mut cur_dy = dy
  for _step in 0..<max_steps {
    if cx < 0 || cx >= grid.width || cy < 0 || cy >= grid.height || cz < 0 ||
      cz >= grid.depth {
      return
    }
    grid.set(cx, cy, cz, @types.Corridor)
    // Occasionally descend to next layer
    if cz + 1 < grid.depth && rng.rand_bool(0.15) {
      cz += 1
      grid.set(cx, cy, cz, @types.Corridor)
    }
    // Random direction wobble
    if rng.rand_bool(0.3) {
      // Swap or flip direction for organic feel
      if rng.rand_bool(0.5) {
        let tmp = cur_dx
        cur_dx = cur_dy
        cur_dy = tmp
      } else {
        cur_dx = -cur_dx
      }
    }
    // Move
    cx += cur_dx
    cy += cur_dy
    // Branch
    if rng.rand_bool(branch_chance) {
      let bdx = if rng.rand_bool(0.5) { 1 } else { -1 }
      let bdy = if rng.rand_bool(0.5) { 1 } else { -1 }
      grow_root(
        grid,
        cx,
        cy,
        cz,
        bdx,
        bdy,
        rng,
        branch_chance * 0.5,
        rooms,
        room_id_counter,
        max_steps / 2,
      )
    }
  }
  // Place nutrient chamber at root tip
  if cx >= 2 && cx < grid.width - 2 && cy >= 2 && cy < grid.height - 2 && cz >= 0 &&
    cz < grid.depth {
    let chamber_w = rng.rand_range(2, 4)
    let chamber_h = rng.rand_range(2, 4)
    let rx = cx - chamber_w / 2
    let ry = cy - chamber_h / 2
    if rx >= 0 && ry >= 0 && rx + chamber_w < grid.width && ry + chamber_h < grid.height {
      for dy in 0..<chamber_h {
        for dx in 0..<chamber_w {
          grid.set(rx + dx, ry + dy, cz, @types.Floor)
        }
      }
      let rid = room_id_counter[0]
      room_id_counter[0] = rid + 1
      rooms.push(
        { rect: { x: rx, y: ry, w: chamber_w, h: chamber_h }, id: rid },
      )
    }
  }
}

///|
pub fn generate(
  config : TreeRootsConfig,
  rng : @types.RNG
) -> TreeRootsResult {
  let width = config.width
  let height = config.height
  let depth_count = config.depth
  let grid = @types.Grid3D::make(width, height, depth_count, @types.Wall)
  let rooms : Array[@types.Room] = []
  let room_id_counter : Array[Int] = [0]
  // Layer 0: surface with tree trunk at center
  let cx = width / 2
  let cy = height / 2
  // Fill surface layer with Floor
  for y in 0..<height {
    for x in 0..<width {
      grid.set(x, y, 0, @types.Floor)
    }
  }
  // Draw trunk at center
  let trunk_size = 3
  let trunk_x = cx - trunk_size / 2
  let trunk_y = cy - trunk_size / 2
  for dy in 0..<trunk_size {
    for dx in 0..<trunk_size {
      let tx = trunk_x + dx
      let ty = trunk_y + dy
      if tx >= 0 && tx < width && ty >= 0 && ty < height {
        grid.set(tx, ty, 0, @types.Start)
      }
    }
  }
  // Surface room
  rooms.push(
    { rect: { x: 0, y: 0, w: width, h: height }, id: room_id_counter[0] },
  )
  room_id_counter[0] = room_id_counter[0] + 1
  // Grow roots downward from trunk base
  // Start roots from around the trunk on layer 1
  let root_dirs : Array[(Int, Int)] = [
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
    (1, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
  ]
  let max_steps = width + height
  for dir in root_dirs {
    let start_x = cx + dir.0 * 2
    let start_y = cy + dir.1 * 2
    if start_x >= 0 && start_x < width && start_y >= 0 && start_y < height {
      // Connect surface to underground
      if depth_count > 1 {
        grid.set(start_x, start_y, 1, @types.Corridor)
      }
      grow_root(
        grid,
        start_x,
        start_y,
        1,
        dir.0,
        dir.1,
        rng,
        config.branch_chance,
        rooms,
        room_id_counter,
        max_steps,
      )
    }
  }
  // Ensure vertical connections between layers under trunk
  for z in 0..<(depth_count - 1) {
    grid.set(cx, cy, z, @types.Corridor)
    grid.set(cx, cy, z + 1, @types.Corridor)
  }
  { grid, rooms }
}
