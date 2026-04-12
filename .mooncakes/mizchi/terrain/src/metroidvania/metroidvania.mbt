///|
pub(all) struct MetroidvaniaConfig {
  width : Int
  height : Int
  num_zones : Int
  rooms_per_zone : Int
  gate_chance : Double
  room_min_size : Int
  room_max_size : Int
}

///|
pub fn MetroidvaniaConfig::default() -> MetroidvaniaConfig {
  {
    width: 60,
    height: 35,
    num_zones: 4,
    rooms_per_zone: 4,
    gate_chance: 0.3,
    room_min_size: 4,
    room_max_size: 8,
  }
}

///|
pub(all) struct MetroidvaniaResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
  edges : Array[@types.Edge]
}

///| Represents a zone as a rectangular region of the grid.
priv struct Zone {
  bounds : @types.Rect
  room_ids : Array[Int]
}

///| Phase 1: Divide the grid into zones (horizontal strips).
fn create_zones(config : MetroidvaniaConfig) -> Array[Zone] {
  let zones : Array[Zone] = []
  let zone_height = config.height / config.num_zones
  for i in 0..<config.num_zones {
    let y = i * zone_height
    let h = if i == config.num_zones - 1 {
      config.height - y
    } else {
      zone_height
    }
    ignore(i)
    zones.push({
      bounds: { x: 0, y, w: config.width, h },
      room_ids: [],
    })
  }
  zones
}

///| Phase 2: Place rooms within each zone, avoiding overlap.
fn place_zone_rooms(
  zones : Array[Zone],
  config : MetroidvaniaConfig,
  rng : @types.RNG
) -> Array[@types.Room] {
  let all_rooms : Array[@types.Room] = []
  let margin = 2
  for zone in zones {
    let zone_x_min = zone.bounds.x + margin
    let zone_y_min = zone.bounds.y + margin
    let zone_x_max = zone.bounds.x + zone.bounds.w - margin
    let zone_y_max = zone.bounds.y + zone.bounds.h - margin
    for _ in 0..<config.rooms_per_zone {
      let rw = rng.rand_range(config.room_min_size, config.room_max_size + 1)
      let rh = rng.rand_range(config.room_min_size, config.room_max_size + 1)
      let mut placed = false
      for _ in 0..<60 {
        let max_x = (zone_x_max - rw).max(zone_x_min)
        let max_y = (zone_y_max - rh).max(zone_y_min)
        let x = rng.rand_range(zone_x_min, max_x + 1)
        let y = rng.rand_range(zone_y_min, max_y + 1)
        let rect : @types.Rect = { x, y, w: rw, h: rh }
        // Check overlap with all existing rooms
        let mut overlaps = false
        for existing in all_rooms {
          let padded : @types.Rect = {
            x: existing.rect.x - 2,
            y: existing.rect.y - 2,
            w: existing.rect.w + 4,
            h: existing.rect.h + 4,
          }
          if rect.intersects(padded) {
            overlaps = true
            break
          }
        }
        if not(overlaps) {
          let room_id = all_rooms.length()
          all_rooms.push({ rect, id: room_id })
          zone.room_ids.push(room_id)
          placed = true
          break
        }
      }
      if not(placed) {
        // Fallback placement
        let x = rng.rand_range(zone_x_min, (zone_x_max - rw).max(zone_x_min) + 1)
        let y = rng.rand_range(zone_y_min, (zone_y_max - rh).max(zone_y_min) + 1)
        let room_id = all_rooms.length()
        all_rooms.push({ rect: { x, y, w: rw, h: rh }, id: room_id })
        zone.room_ids.push(room_id)
      }
    }
  }
  all_rooms
}

///| Phase 3: Connect rooms within zones and between zones. Returns edges and
/// a set of locked edge indices.
fn connect_zones(
  zones : Array[Zone],
  rooms : Array[@types.Room],
  config : MetroidvaniaConfig,
  rng : @types.RNG
) -> (Array[@types.Edge], Array[Bool]) {
  let edges : Array[@types.Edge] = []
  let is_locked : Array[Bool] = []
  // Connect rooms within each zone in a chain
  for zone in zones {
    if zone.room_ids.length() < 2 {
      continue
    }
    for i in 0..<(zone.room_ids.length() - 1) {
      edges.push({ from: zone.room_ids[i], to: zone.room_ids[i + 1] })
      is_locked.push(false)
    }
    // Add one extra connection for variety (last to first in zone)
    if zone.room_ids.length() >= 3 && rng.rand_bool(0.4) {
      let a = zone.room_ids[0]
      let b = zone.room_ids[zone.room_ids.length() - 1]
      edges.push({ from: a, to: b })
      is_locked.push(false)
    }
  }
  // Connect adjacent zones: pick border rooms from each and connect
  for i in 0..<(zones.length() - 1) {
    let zone_a = zones[i]
    let zone_b = zones[i + 1]
    if zone_a.room_ids.length() == 0 || zone_b.room_ids.length() == 0 {
      continue
    }
    // Pick the room in zone_a closest to zone_b boundary, and vice versa
    let border_y = zone_b.bounds.y
    let mut best_a = zone_a.room_ids[0]
    let mut best_a_dist = 99999
    for rid in zone_a.room_ids {
      let center = rooms[rid].rect.center()
      let dist = (center.y - border_y).abs()
      if dist < best_a_dist {
        best_a_dist = dist
        best_a = rid
      }
    }
    let mut best_b = zone_b.room_ids[0]
    let mut best_b_dist = 99999
    for rid in zone_b.room_ids {
      let center = rooms[rid].rect.center()
      let dist = (center.y - border_y).abs()
      if dist < best_b_dist {
        best_b_dist = dist
        best_b = rid
      }
    }
    edges.push({ from: best_a, to: best_b })
    // Gate between zones with gate_chance probability
    let locked = rng.rand_bool(config.gate_chance)
    is_locked.push(locked)
  }
  (edges, is_locked)
}

///| Phase 4: Render everything to the grid.
fn render_metroidvania_grid(
  zones : Array[Zone],
  rooms : Array[@types.Room],
  edges : Array[@types.Edge],
  is_locked : Array[Bool],
  config : MetroidvaniaConfig
) -> @types.Grid2D {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  // Carve rooms
  for r in rooms {
    grid.fill_rect(r.rect, @types.Floor)
  }
  // Carve corridors
  for i, edge in edges {
    let from_center = rooms[edge.from].rect.center()
    let to_center = rooms[edge.to].rect.center()
    if i < is_locked.length() && is_locked[i] {
      // Locked corridor: carve and place LockedDoor at bend
      let mid : @types.Point = { x: to_center.x, y: from_center.y }
      grid.carve_line(from_center, mid, @types.Corridor)
      grid.carve_line(mid, to_center, @types.Corridor)
      grid.set(mid.x, mid.y, @types.LockedDoor)
    } else {
      grid.carve_l_corridor(from_center, to_center, @types.Corridor)
    }
  }
  // Place keys: for each locked inter-zone edge, place a key in the previous zone.
  // Locked inter-zone edges are the last (num_zones-1) edges.
  // We need to find which inter-zone edges are locked.
  let intra_edge_count = edges.length() - (zones.length() - 1)
  for i in 0..<(zones.length() - 1) {
    let edge_idx = intra_edge_count + i
    if edge_idx < is_locked.length() && is_locked[edge_idx] {
      // Place key in zone i (the zone before the lock)
      let zone = zones[i]
      if zone.room_ids.length() > 0 {
        // Pick a room in this zone that's not the border room used for connection
        let key_room_id = if zone.room_ids.length() > 1 {
          // Prefer a room that's not the connection room
          let connection_room = edges[edge_idx].from
          let mut picked = zone.room_ids[0]
          for rid in zone.room_ids {
            if rid != connection_room {
              picked = rid
              break
            }
          }
          picked
        } else {
          zone.room_ids[0]
        }
        let center = rooms[key_room_id].rect.center()
        grid.set(center.x, center.y, @types.Key)
      }
    }
  }
  // Place Start in the first room of zone 0
  if zones[0].room_ids.length() > 0 {
    let start_room = rooms[zones[0].room_ids[0]]
    let center = start_room.rect.center()
    grid.set(center.x, center.y, @types.Start)
  }
  // Place Goal in the last room of the last zone
  let last_zone = zones[zones.length() - 1]
  if last_zone.room_ids.length() > 0 {
    let goal_room = rooms[last_zone.room_ids[last_zone.room_ids.length() - 1]]
    let center = goal_room.rect.center()
    grid.set(center.x, center.y, @types.Goal)
  }
  grid
}

///|
pub fn generate(
  config : MetroidvaniaConfig,
  rng : @types.RNG
) -> MetroidvaniaResult {
  let zones = create_zones(config)
  let rooms = place_zone_rooms(zones, config, rng)
  let (edges, is_locked) = connect_zones(zones, rooms, config, rng)
  let grid = render_metroidvania_grid(zones, rooms, edges, is_locked, config)
  { grid, rooms, edges }
}
