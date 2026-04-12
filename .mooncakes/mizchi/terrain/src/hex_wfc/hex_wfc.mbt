///|
pub(all) struct HexWfcConfig {
  cols : Int
  rows : Int
  max_retries : Int
}

///|
pub fn HexWfcConfig::default() -> HexWfcConfig {
  { cols: 20, rows: 15, max_retries: 10 }
}

///|
pub(all) struct HexWfcResult {
  grid : @types.HexGrid
  success : Bool
}

///|
priv struct CellState {
  mut can_floor : Bool
  mut can_wall : Bool
  mut collapsed : Bool
  mut tile : @types.Tile
}

///|
fn make_cell_state() -> CellState {
  { can_floor: true, can_wall: true, collapsed: false, tile: @types.Empty }
}

///|
fn CellState::entropy(self : CellState) -> Int {
  if self.collapsed {
    return 0
  }
  let mut count = 0
  if self.can_floor {
    count += 1
  }
  if self.can_wall {
    count += 1
  }
  count
}

///|
pub fn generate(config : HexWfcConfig, rng : @types.RNG) -> HexWfcResult {
  let cols = config.cols
  let rows = config.rows
  let total = cols * rows
  for _retry in 0..<config.max_retries {
    let hex = @types.HexGrid::make(cols, rows, @types.Empty)
    let states : Array[CellState] = []
    for _i in 0..<total {
      states.push(make_cell_state())
    }
    let mut success = true
    let mut remaining = total
    while remaining > 0 {
      // Find uncollapsed cell with lowest entropy (> 0)
      let mut min_entropy = 3
      let mut candidates : Array[Int] = []
      for i in 0..<total {
        if states[i].collapsed {
          continue
        }
        let e = states[i].entropy()
        if e == 0 {
          // Contradiction: no options left
          success = false
          break
        }
        if e < min_entropy {
          min_entropy = e
          candidates = [i]
        } else if e == min_entropy {
          candidates.push(i)
        }
      }
      if not(success) || candidates.length() == 0 {
        break
      }
      // Pick random cell among lowest entropy
      let idx = rng.rand_pick(candidates)
      let state = states[idx]
      // Collapse: pick a tile
      if state.can_floor && state.can_wall {
        // Bias slightly towards floor for better connectivity
        if rng.rand_bool(0.55) {
          state.tile = @types.Floor
          state.can_wall = false
        } else {
          state.tile = @types.Wall
          state.can_floor = false
        }
      } else if state.can_floor {
        state.tile = @types.Floor
      } else {
        state.tile = @types.Wall
      }
      state.collapsed = true
      remaining -= 1
      let col = idx % cols
      let row = idx / cols
      hex.set(col, row, state.tile)
      // Propagate constraints to neighbors
      propagate(hex, states, cols, rows, col, row)
    }
    if success && remaining == 0 {
      return { grid: hex, success: true }
    }
  }
  // All retries failed, return last attempt
  let grid = @types.HexGrid::make(cols, rows, @types.Floor)
  { grid, success: false }
}

///|
fn propagate(
  hex : @types.HexGrid,
  states : Array[CellState],
  cols : Int,
  _rows : Int,
  col : Int,
  row : Int
) -> Unit {
  // BFS propagation from collapsed cell
  let queue : Array[(Int, Int)] = [(col, row)]
  let mut qi = 0
  while qi < queue.length() {
    let current = queue[qi]
    qi += 1
    let cc = current.0
    let cr = current.1
    let neighbors = hex.neighbors6(cc, cr)
    for n in neighbors {
      let nc = n.0
      let nr = n.1
      let nidx = nr * cols + nc
      let nstate = states[nidx]
      if nstate.collapsed {
        continue
      }
      let mut changed = false
      // Constraint: if all collapsed neighbors are Wall, this cell should prefer Floor
      // to avoid isolated wall clusters
      let all_neighbors = hex.neighbors6(nc, nr)
      let mut wall_neighbor_count = 0
      let mut collapsed_neighbor_count = 0
      for nn in all_neighbors {
        let nn_idx = nn.1 * cols + nn.0
        if states[nn_idx].collapsed {
          collapsed_neighbor_count += 1
          if states[nn_idx].tile == @types.Wall {
            wall_neighbor_count += 1
          }
        }
      }
      // If all collapsed neighbors are walls, forbid wall (prevent large wall blobs)
      if collapsed_neighbor_count >= 3 && wall_neighbor_count == collapsed_neighbor_count {
        if nstate.can_wall {
          nstate.can_wall = false
          changed = true
        }
      }
      // If too many floor neighbors, allow wall to create some structure
      let floor_count = collapsed_neighbor_count - wall_neighbor_count
      if collapsed_neighbor_count >= 4 && floor_count == collapsed_neighbor_count {
        // All neighbors are floor - still allow both options
        ignore(floor_count)
      }
      if changed {
        queue.push((nc, nr))
      }
    }
  }
}
