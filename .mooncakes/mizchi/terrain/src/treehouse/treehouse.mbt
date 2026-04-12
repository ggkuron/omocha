///|
pub(all) struct TreehouseConfig {
  width : Int
  height : Int
  max_height : Int
  num_trees : Int
  bridge_chance : Double
}

///|
pub fn TreehouseConfig::default() -> TreehouseConfig {
  { width: 48, height: 48, max_height: 8, num_trees: 8, bridge_chance: 0.5 }
}

///|
pub(all) struct TreehouseResult {
  grid : @types.HeightGrid
  rooms : Array[@types.Room]
}

///|
pub fn generate(config : TreehouseConfig, rng : @types.RNG) -> TreehouseResult {
  let w = config.width
  let h = config.height
  let grid = @types.HeightGrid::make(w, h, @types.Empty, config.max_height)
  let rooms : Array[@types.Room] = []
  // Fill ground level with low underbrush
  for y in 0..<h {
    for x in 0..<w {
      if rng.rand_bool(0.3) {
        grid.set(x, y, @types.Wall)
        grid.set_height(x, y, 1)
      } else {
        grid.set(x, y, @types.Floor)
        grid.set_height(x, y, 0)
      }
    }
  }
  // Tree data
  let tree_centers : Array[@types.Point] = []
  let tree_heights : Array[Int] = []
  let platform_rects : Array[@types.Rect] = []
  let mut room_id = 0
  // Place trees
  for _i in 0..<config.num_trees {
    let mut placed = false
    let mut attempts = 0
    while not(placed) && attempts < 200 {
      attempts += 1
      let cx = rng.rand_range(5, w - 5)
      let cy = rng.rand_range(5, h - 5)
      // Check distance from other trees
      let mut too_close = false
      for tc in tree_centers {
        let dx = cx - tc.x
        let dy = cy - tc.y
        if isqrt(dx * dx + dy * dy) < 8 {
          too_close = true
          break
        }
      }
      if not(too_close) {
        let tree_h = rng.rand_range(config.max_height - 2, config.max_height + 1)
        tree_centers.push({ x: cx, y: cy })
        tree_heights.push(tree_h)
        // Draw tree trunk (2x2 Wall column at full height)
        for dy in -1..=0 {
          for dx in -1..=0 {
            let tx = cx + dx
            let ty = cy + dy
            if tx >= 0 && tx < w && ty >= 0 && ty < h {
              grid.set(tx, ty, @types.Wall)
              grid.set_height(tx, ty, tree_h)
            }
          }
        }
        // Draw canopy (irregular circle around trunk at high height)
        let canopy_r = rng.rand_range(2, 4)
        for dy in -canopy_r..=canopy_r {
          for dx in -canopy_r..=canopy_r {
            let dist_sq = dx * dx + dy * dy
            if dist_sq <= canopy_r * canopy_r {
              let px = cx + dx
              let py = cy + dy
              if px >= 0 && px < w && py >= 0 && py < h {
                let cur = grid.get(px, py)
                if cur != @types.Wall || grid.get_height(px, py) < tree_h {
                  // Canopy is Wall at tree height - 1
                  grid.set(px, py, @types.Wall)
                  grid.set_height(px, py, tree_h - 1)
                }
              }
            }
          }
        }
        // Create platform near the trunk
        let plat_w = rng.rand_range(3, 6)
        let plat_h = rng.rand_range(3, 5)
        // Offset platform slightly from trunk center
        let plat_dir_x = rng.rand_range(-1, 2)
        let plat_dir_y = rng.rand_range(-1, 2)
        let plat_x = cx + plat_dir_x * 2 - plat_w / 2
        let plat_y = cy + plat_dir_y * 2 - plat_h / 2
        let plat_height = rng.rand_range(tree_h / 2, tree_h - 1)
        let rect : @types.Rect = { x: plat_x, y: plat_y, w: plat_w, h: plat_h }
        // Draw platform
        for py in plat_y..<(plat_y + plat_h) {
          for px in plat_x..<(plat_x + plat_w) {
            if px >= 0 && px < w && py >= 0 && py < h {
              // Platform border is wall, interior is floor
              let is_border = px == plat_x || px == plat_x + plat_w - 1 ||
                py == plat_y || py == plat_y + plat_h - 1
              if is_border {
                grid.set(px, py, @types.Wall)
                grid.set_height(px, py, plat_height)
              } else {
                grid.set(px, py, @types.Floor)
                grid.set_height(px, py, plat_height)
              }
            }
          }
        }
        // Add door to platform
        let door_side = rng.next_int(4)
        let door_x = match door_side {
          0 => plat_x
          1 => plat_x + plat_w - 1
          _ => plat_x + plat_w / 2
        }
        let door_y = match door_side {
          2 => plat_y
          3 => plat_y + plat_h - 1
          _ => plat_y + plat_h / 2
        }
        if door_x >= 0 && door_x < w && door_y >= 0 && door_y < h {
          grid.set(door_x, door_y, @types.Door)
          grid.set_height(door_x, door_y, plat_height)
        }
        platform_rects.push(rect)
        rooms.push({ rect, id: room_id })
        room_id += 1
        placed = true
      }
    }
  }
  // Connect platforms with rope bridges
  // First connect via MST
  if tree_centers.length() > 1 {
    let n = tree_centers.length()
    let in_mst = Array::make(n, false)
    let min_dist = Array::make(n, 999999)
    let min_edge = Array::make(n, 0)
    in_mst[0] = true
    for i in 1..<n {
      let d = dist_sq(tree_centers[0], tree_centers[i])
      min_dist[i] = d
      min_edge[i] = 0
    }
    let mst_edges : Array[(Int, Int)] = []
    for _iter in 1..<n {
      let mut best = -1
      let mut best_d = 999999
      for j in 0..<n {
        if not(in_mst[j]) && min_dist[j] < best_d {
          best_d = min_dist[j]
          best = j
        }
      }
      if best < 0 {
        break
      }
      in_mst[best] = true
      mst_edges.push((min_edge[best], best))
      for j in 0..<n {
        if not(in_mst[j]) {
          let d = dist_sq(tree_centers[best], tree_centers[j])
          if d < min_dist[j] {
            min_dist[j] = d
            min_edge[j] = best
          }
        }
      }
    }
    // Draw MST bridges
    for edge in mst_edges {
      let from_idx = edge.0
      let to_idx = edge.1
      let from_rect = platform_rects[from_idx]
      let to_rect = platform_rects[to_idx]
      let from_c = from_rect.center()
      let to_c = to_rect.center()
      let from_h = grid.get_height(
        from_c.x.max(0).min(w - 1),
        from_c.y.max(0).min(h - 1),
      )
      let to_h = grid.get_height(
        to_c.x.max(0).min(w - 1),
        to_c.y.max(0).min(h - 1),
      )
      build_rope_bridge(grid, from_c, to_c, from_h, to_h, w, h)
    }
    // Extra random bridges
    for i in 0..<n {
      for j in (i + 1)..<n {
        let mut is_mst = false
        for e in mst_edges {
          if (e.0 == i && e.1 == j) || (e.0 == j && e.1 == i) {
            is_mst = true
            break
          }
        }
        if not(is_mst) && rng.rand_bool(config.bridge_chance) {
          let from_rect = platform_rects[i]
          let to_rect = platform_rects[j]
          let from_c = from_rect.center()
          let to_c = to_rect.center()
          let from_h = grid.get_height(
            from_c.x.max(0).min(w - 1),
            from_c.y.max(0).min(h - 1),
          )
          let to_h = grid.get_height(
            to_c.x.max(0).min(w - 1),
            to_c.y.max(0).min(h - 1),
          )
          build_rope_bridge(grid, from_c, to_c, from_h, to_h, w, h)
        }
      }
    }
  }
  // Place Start and Goal on platforms
  if platform_rects.length() > 0 {
    let start_rect = platform_rects[0]
    let start_c = start_rect.center()
    let sx = start_c.x.max(0).min(w - 1)
    let sy = start_c.y.max(0).min(h - 1)
    grid.set(sx, sy, @types.Start)
    let goal_rect = platform_rects[platform_rects.length() - 1]
    let goal_c = goal_rect.center()
    let gx = goal_c.x.max(0).min(w - 1)
    let gy = goal_c.y.max(0).min(h - 1)
    grid.set(gx, gy, @types.Goal)
  }
  { grid, rooms }
}

///|
fn build_rope_bridge(
  grid : @types.HeightGrid,
  from : @types.Point,
  to : @types.Point,
  from_h : Int,
  to_h : Int,
  w : Int,
  h : Int
) -> Unit {
  // L-shaped corridor bridge
  let mut x = from.x
  let mut y = from.y
  let total_steps = abs(to.x - from.x) + abs(to.y - from.y)
  if total_steps == 0 {
    return
  }
  let mut step = 0
  // Horizontal segment
  while x != to.x {
    if x >= 0 && x < w && y >= 0 && y < h {
      let t = step.to_double() / total_steps.to_double()
      // Rope bridge sags in the middle
      let mid_sag = 1
      let sag = if t > 0.3 && t < 0.7 { mid_sag } else { 0 }
      let bridge_h = from_h +
        ((to_h - from_h).to_double() * t).to_int() -
        sag
      let bridge_h_clamped = bridge_h.max(1)
      let cur = grid.get(x, y)
      if cur == @types.Empty || (cur == @types.Floor && grid.get_height(x, y) == 0) {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, bridge_h_clamped)
      }
    }
    x = x + (if to.x > from.x { 1 } else { -1 })
    step += 1
  }
  // Vertical segment
  while y != to.y {
    if x >= 0 && x < w && y >= 0 && y < h {
      let t = step.to_double() / total_steps.to_double()
      let mid_sag = 1
      let sag = if t > 0.3 && t < 0.7 { mid_sag } else { 0 }
      let bridge_h = from_h +
        ((to_h - from_h).to_double() * t).to_int() -
        sag
      let bridge_h_clamped = bridge_h.max(1)
      let cur = grid.get(x, y)
      if cur == @types.Empty || (cur == @types.Floor && grid.get_height(x, y) == 0) {
        grid.set(x, y, @types.Corridor)
        grid.set_height(x, y, bridge_h_clamped)
      }
    }
    y = y + (if to.y > from.y { 1 } else { -1 })
    step += 1
  }
}

///|
fn dist_sq(a : @types.Point, b : @types.Point) -> Int {
  let dx = a.x - b.x
  let dy = a.y - b.y
  dx * dx + dy * dy
}

///|
fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

///|
fn isqrt(n : Int) -> Int {
  if n <= 0 {
    return 0
  }
  let mut x = n
  let mut y = (x + 1) / 2
  while y < x {
    x = y
    y = (x + n / x) / 2
  }
  x
}
