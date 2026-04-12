///|
pub(all) struct GridMetrics {
  open_ratio : Double
  room_count : Int
  connectivity : Bool
  largest_component : Int
  dead_end_count : Int
} derive(Show)

///|
fn is_walkable(tile : Tile) -> Bool {
  match tile {
    Floor | Door | LockedDoor | Key | Start | Goal | Corridor => true
    _ => false
  }
}

///|
pub fn flood_fill(grid : Grid2D, start_x : Int, start_y : Int) -> Array[Point] {
  let visited : Array[Point] = []
  let seen = Array::make(grid.width * grid.height, false)
  let stack : Array[Point] = [{ x: start_x, y: start_y }]
  seen[start_y * grid.width + start_x] = true
  while stack.length() > 0 {
    let p = stack.pop().unwrap()
    visited.push(p)
    let neighbors = grid.neighbors4(p.x, p.y)
    for n in neighbors {
      let idx = n.y * grid.width + n.x
      if not(seen[idx]) && is_walkable(grid.get(n.x, n.y)) {
        seen[idx] = true
        stack.push(n)
      }
    }
  }
  visited
}

///|
pub fn compute_metrics(grid : Grid2D, room_count : Int) -> GridMetrics {
  let total = grid.width * grid.height
  let mut walkable = 0
  let mut dead_ends = 0
  let mut first_walkable : Point? = None
  for y in 0..<grid.height {
    for x in 0..<grid.width {
      if is_walkable(grid.get(x, y)) {
        walkable += 1
        if first_walkable is None {
          first_walkable = Some({ x, y })
        }
        // Count dead ends: walkable with exactly 1 walkable neighbor
        let mut adj = 0
        let neighbors = grid.neighbors4(x, y)
        for n in neighbors {
          if is_walkable(grid.get(n.x, n.y)) {
            adj += 1
          }
        }
        if adj == 1 {
          dead_ends += 1
        }
      }
    }
  }
  let open_ratio = if total > 0 {
    walkable.to_double() / total.to_double()
  } else {
    0.0
  }
  let (connectivity, largest) = match first_walkable {
    Some(p) => {
      let component = flood_fill(grid, p.x, p.y)
      (component.length() == walkable, component.length())
    }
    None => (true, 0)
  }
  {
    open_ratio,
    room_count,
    connectivity,
    largest_component: largest,
    dead_end_count: dead_ends,
  }
}
