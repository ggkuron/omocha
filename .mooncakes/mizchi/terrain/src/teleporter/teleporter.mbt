///|
pub(all) struct TeleporterConfig {
  width : Int
  height : Int
  num_clusters : Int
  teleporters_per_cluster : Int
}

///|
pub fn TeleporterConfig::default() -> TeleporterConfig {
  { width: 50, height: 30, num_clusters: 4, teleporters_per_cluster: 2 }
}

///|
pub(all) struct TeleporterResult {
  grid : @types.Grid2D
  rooms : Array[@types.Room]
}

///| Divide the map into cluster regions (vertical strips)
fn compute_cluster_regions(
  config : TeleporterConfig
) -> Array[@types.Rect] {
  let regions : Array[@types.Rect] = []
  let strip_w = (config.width - 2) / config.num_clusters
  for i in 0..<config.num_clusters {
    let x = 1 + i * strip_w
    let w = if i == config.num_clusters - 1 {
      config.width - 1 - x
    } else {
      strip_w
    }
    regions.push({ x, y: 1, w, h: config.height - 2 })
  }
  regions
}

///| Place 2-3 rooms within a cluster region
fn place_cluster_rooms(
  region : @types.Rect,
  rooms : Array[@types.Room],
  rng : @types.RNG
) -> Array[Int] {
  let room_ids : Array[Int] = []
  let num_rooms_in_cluster = rng.rand_range(2, 4)
  let min_size = 4
  let max_size = 6
  for _ in 0..<num_rooms_in_cluster {
    let rw = rng.rand_range(min_size, max_size + 1)
    let rh = rng.rand_range(min_size, max_size + 1)
    // Clamp to region
    let actual_w = if rw > region.w - 2 { region.w - 2 } else { rw }
    let actual_h = if rh > region.h - 2 { region.h - 2 } else { rh }
    if actual_w < min_size || actual_h < min_size {
      continue
    }
    let mut placed = false
    for _ in 0..<40 {
      let rx = rng.rand_range(region.x + 1, region.x + region.w - actual_w)
      let ry = rng.rand_range(region.y + 1, region.y + region.h - actual_h)
      let rect : @types.Rect = { x: rx, y: ry, w: actual_w, h: actual_h }
      let mut overlaps = false
      for existing in rooms {
        let padded : @types.Rect = {
          x: existing.rect.x - 1,
          y: existing.rect.y - 1,
          w: existing.rect.w + 2,
          h: existing.rect.h + 2,
        }
        if rect.intersects(padded) {
          overlaps = true
          break
        }
      }
      if not(overlaps) {
        let id = rooms.length()
        rooms.push({ rect, id })
        room_ids.push(id)
        placed = true
        break
      }
    }
    if not(placed) {
      // Fallback
      let rx = rng.rand_range(region.x + 1, region.x + region.w - actual_w)
      let ry = rng.rand_range(region.y + 1, region.y + region.h - actual_h)
      let id = rooms.length()
      rooms.push({ rect: { x: rx, y: ry, w: actual_w, h: actual_h }, id })
      room_ids.push(id)
    }
  }
  room_ids
}

///| Connect rooms within a cluster using corridors
fn connect_cluster_rooms(
  grid : @types.Grid2D,
  rooms : Array[@types.Room],
  room_ids : Array[Int]
) -> Unit {
  for i in 0..<(room_ids.length() - 1) {
    let from = rooms[room_ids[i]].rect.center()
    let to = rooms[room_ids[i + 1]].rect.center()
    grid.carve_l_corridor(from, to, @types.Corridor)
  }
}

///| Place teleporter pads (Key tiles) linking two clusters
fn place_teleporter_pair(
  grid : @types.Grid2D,
  rooms : Array[@types.Room],
  cluster_a_ids : Array[Int],
  cluster_b_ids : Array[Int],
  rng : @types.RNG
) -> Unit {
  if cluster_a_ids.length() == 0 || cluster_b_ids.length() == 0 {
    return
  }
  // Pick a room from each cluster
  let room_a_id = rng.rand_pick(cluster_a_ids)
  let room_b_id = rng.rand_pick(cluster_b_ids)
  let room_a = rooms[room_a_id]
  let room_b = rooms[room_b_id]
  // Place teleporter pad (Key tile) near room center in each
  let pos_a = find_teleporter_spot(grid, room_a, rng)
  let pos_b = find_teleporter_spot(grid, room_b, rng)
  grid.set(pos_a.x, pos_a.y, @types.Key)
  grid.set(pos_b.x, pos_b.y, @types.Key)
}

///| Find a floor tile near the room edge for teleporter placement
fn find_teleporter_spot(
  grid : @types.Grid2D,
  room : @types.Room,
  rng : @types.RNG
) -> @types.Point {
  // Try random positions inside the room
  for _ in 0..<20 {
    let x = rng.rand_range(room.rect.x + 1, room.rect.x + room.rect.w - 1)
    let y = rng.rand_range(room.rect.y + 1, room.rect.y + room.rect.h - 1)
    if grid.in_bounds(x, y) && grid.get(x, y) == @types.Floor {
      return { x, y }
    }
  }
  // Fallback: room center
  room.rect.center()
}

///|
pub fn generate(
  config : TeleporterConfig,
  rng : @types.RNG
) -> TeleporterResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let rooms : Array[@types.Room] = []
  let regions = compute_cluster_regions(config)
  // Build room IDs per cluster
  let cluster_room_ids : Array[Array[Int]] = []
  for region in regions {
    let ids = place_cluster_rooms(region, rooms, rng)
    cluster_room_ids.push(ids)
  }
  // Carve all rooms
  for room in rooms {
    grid.fill_rect(room.rect, @types.Floor)
  }
  // Connect rooms within each cluster
  for ids in cluster_room_ids {
    connect_cluster_rooms(grid, rooms, ids)
  }
  // Place teleporter pairs between adjacent clusters
  for i in 0..<cluster_room_ids.length() {
    for _ in 0..<config.teleporters_per_cluster {
      // Connect to the next cluster (wrapping)
      let next = (i + 1) % cluster_room_ids.length()
      if next != i {
        place_teleporter_pair(
          grid, rooms, cluster_room_ids[i], cluster_room_ids[next], rng,
        )
      }
    }
  }
  // Place Start in first cluster, Goal in last cluster
  if cluster_room_ids.length() > 0 {
    let first_ids = cluster_room_ids[0]
    if first_ids.length() > 0 {
      let start_center = rooms[first_ids[0]].rect.center()
      grid.set(start_center.x, start_center.y, @types.Start)
    }
    let last_ids = cluster_room_ids[cluster_room_ids.length() - 1]
    if last_ids.length() > 0 {
      let goal_center = rooms[last_ids[last_ids.length() - 1]].rect.center()
      grid.set(goal_center.x, goal_center.y, @types.Goal)
    }
  }
  { grid, rooms }
}
