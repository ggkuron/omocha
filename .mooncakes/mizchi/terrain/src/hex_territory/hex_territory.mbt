///|
pub(all) struct HexTerritoryConfig {
  cols : Int
  rows : Int
  num_factions : Int
  aggression : Double
}

///|
pub fn HexTerritoryConfig::default() -> HexTerritoryConfig {
  { cols: 30, rows: 20, num_factions: 4, aggression: 0.3 }
}

///|
pub(all) struct HexTerritoryResult {
  grid : @types.HexGrid
  rooms : Array[@types.Room]
}

///|
fn faction_tile(faction_id : Int) -> @types.Tile {
  // Different factions use different tile types
  match faction_id % 5 {
    0 => @types.Floor
    1 => @types.Corridor
    2 => @types.Water
    3 => @types.Key
    _ => @types.Door
  }
}

///|
pub fn generate(
  config : HexTerritoryConfig,
  rng : @types.RNG
) -> HexTerritoryResult {
  let cols = config.cols
  let rows = config.rows
  let total = cols * rows
  let grid = @types.HexGrid::make(cols, rows, @types.Wall)
  let rooms : Array[@types.Room] = []
  let num_factions = if config.num_factions < 1 {
    1
  } else {
    config.num_factions
  }
  // Ownership map: -1 = unclaimed
  let owner = Array::make(total, -1)
  // Place faction seed points
  let faction_seeds : Array[(Int, Int)] = []
  for _i in 0..<num_factions {
    let mut attempts = 0
    while attempts < 100 {
      let c = rng.rand_range(1, cols - 1)
      let r = rng.rand_range(1, rows - 1)
      let idx = r * cols + c
      if owner[idx] == -1 {
        let fid = faction_seeds.length()
        owner[idx] = fid
        grid.set(c, r, faction_tile(fid))
        faction_seeds.push((c, r))
        break
      }
      attempts += 1
    }
  }
  let actual_factions = faction_seeds.length()
  if actual_factions == 0 {
    return { grid, rooms }
  }
  // Frontier queues for each faction
  let frontiers : Array[Array[(Int, Int)]] = []
  for i in 0..<actual_factions {
    let seed = faction_seeds[i]
    frontiers.push([seed])
  }
  // Grow factions in rounds
  let max_rounds = cols * rows
  for _round in 0..<max_rounds {
    let mut any_expanded = false
    for fid in 0..<actual_factions {
      let frontier = frontiers[fid]
      if frontier.length() == 0 {
        continue
      }
      // Process current frontier
      let next_frontier : Array[(Int, Int)] = []
      for cell in frontier {
        let neighbors = grid.neighbors6(cell.0, cell.1)
        rng.rand_shuffle(neighbors)
        for n in neighbors {
          let nc = n.0
          let nr = n.1
          let nidx = nr * cols + nc
          // Skip map borders
          if nc == 0 || nr == 0 || nc == cols - 1 || nr == rows - 1 {
            continue
          }
          if owner[nidx] == -1 {
            // Unclaimed: take it
            owner[nidx] = fid
            grid.set(nc, nr, faction_tile(fid))
            next_frontier.push((nc, nr))
            any_expanded = true
          } else if owner[nidx] != fid {
            // Owned by another faction: try aggressive takeover
            if rng.rand_bool(config.aggression) {
              owner[nidx] = fid
              grid.set(nc, nr, faction_tile(fid))
              next_frontier.push((nc, nr))
              any_expanded = true
            }
          }
        }
      }
      frontiers[fid] = next_frontier
    }
    if not(any_expanded) {
      break
    }
  }
  // Build rooms from faction territories
  let faction_min_c = Array::make(actual_factions, cols)
  let faction_min_r = Array::make(actual_factions, rows)
  let faction_max_c = Array::make(actual_factions, 0)
  let faction_max_r = Array::make(actual_factions, 0)
  let faction_has_cells = Array::make(actual_factions, false)
  for r in 0..<rows {
    for c in 0..<cols {
      let fid = owner[r * cols + c]
      if fid >= 0 {
        faction_has_cells[fid] = true
        if c < faction_min_c[fid] {
          faction_min_c[fid] = c
        }
        if c > faction_max_c[fid] {
          faction_max_c[fid] = c
        }
        if r < faction_min_r[fid] {
          faction_min_r[fid] = r
        }
        if r > faction_max_r[fid] {
          faction_max_r[fid] = r
        }
      }
    }
  }
  for i in 0..<actual_factions {
    if faction_has_cells[i] {
      let rect : @types.Rect = {
        x: faction_min_c[i],
        y: faction_min_r[i],
        w: faction_max_c[i] - faction_min_c[i] + 1,
        h: faction_max_r[i] - faction_min_r[i] + 1,
      }
      rooms.push({ rect, id: rooms.length() })
    }
  }
  // Place borders between territories
  for r in 0..<rows {
    for c in 0..<cols {
      let fid = owner[r * cols + c]
      if fid < 0 {
        continue
      }
      let neighbors = grid.neighbors6(c, r)
      for n in neighbors {
        let nfid = owner[n.1 * cols + n.0]
        if nfid >= 0 && nfid != fid {
          grid.set(c, r, @types.Wall)
          break
        }
      }
    }
  }
  { grid, rooms }
}
