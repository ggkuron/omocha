# terrain

117 procedural dungeon/terrain generation algorithms in [MoonBit](https://www.moonbitlang.com/).

[Live Playground](https://mizchi.github.io/terrain/) | [Source](https://github.com/mizchi/terrain)

## Install

```bash
moon add mizchi/terrain
```

## Usage

Each algorithm is an independent package with a `generate(config, rng)` function.

```moonbit
fn main {
  let rng = @terrain/types.make_rng(42U)

  // 2D Grid algorithm
  let result = @terrain/bsp.generate(
    { width: 40, height: 20, min_room_size: 4, max_depth: 4 },
    rng,
  )
  println(result.grid.to_ascii())

  // Hex algorithm
  let rng = @terrain/types.make_rng(42U)
  let hex = @terrain/hex_maze.generate(
    { cols: 20, rows: 15, room_chance: 0.15 },
    rng,
  )
  println(hex.grid.to_ascii())

  // 3D multi-layer algorithm
  let rng = @terrain/types.make_rng(42U)
  let d3 = @terrain/multifloor_bsp.generate(
    { width: 40, height: 20, depth: 3, min_room_size: 4, stair_count: 3 },
    rng,
  )
  // Access each layer
  for z in 0..<d3.grid.depth {
    println("Layer " + z.to_string())
    println(d3.grid.to_ascii(z))
  }

  // Quarter View (isometric) algorithm
  let rng = @terrain/types.make_rng(42U)
  let iso = @terrain/iso_city.generate(
    { width: 40, height: 20, max_height: 8, building_density: 0.7, street_width: 2 },
    rng,
  )
  println(iso.grid.to_ascii())
  // Heights available via iso.grid.heights
}
```

### Core Types

```moonbit
// Tile types
enum Tile { Floor; Wall; Door; LockedDoor; Key; Start; Goal; Corridor; Water; Empty }

// 2D grid (width x height)
struct Grid2D { width : Int; height : Int; cells : Array[Tile] }

// Hex grid (odd-r offset coordinates)
struct HexGrid { cols : Int; rows : Int; cells : Array[Tile]; orientation : Int }

// 3D multi-layer grid
struct Grid3D { width : Int; height : Int; depth : Int; layers : Array[Grid2D] }

// 2D grid with height data (for isometric rendering)
struct HeightGrid { width : Int; height : Int; cells : Array[Tile]; heights : Array[Int]; max_height : Int }

// Room descriptor
struct Room { x : Int; y : Int; w : Int; h : Int }

// Seeded RNG
struct RNG { .. }
fn make_rng(seed : UInt) -> RNG
```

### Pattern

Every algorithm package follows the same pattern:

```moonbit
// Each package exports:
pub(all) struct XxxConfig { .. }     // Config with defaults
pub(all) struct XxxResult { .. }     // Result with grid + rooms
pub fn XxxConfig::default() -> XxxConfig
pub fn generate(config : XxxConfig, rng : @types.RNG) -> XxxResult
```

Import only what you need in `moon.pkg.json`:

```json
{
  "import": [
    "mizchi/terrain/types",
    "mizchi/terrain/bsp",
    "mizchi/terrain/cellular"
  ]
}
```

## Algorithms (117)

### Maze (11)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 0 | Maze (DFS) | `maze` | Recursive backtracker depth-first search |
| 1 | Eller's Maze | `eller` | Row-by-row generation with set merging |
| 2 | Kruskal's Maze | `kruskal` | Minimum spanning tree randomized maze |
| 3 | Binary Tree Maze | `binary_tree` | Two-direction bias with diagonal tendency |
| 4 | Sidewinder Maze | `sidewinder` | Horizontal runs with random vertical connections |
| 5 | Recursive Division | `recursive_division` | Recursive wall placement subdivision |
| 6 | Growing Tree | `growing_tree` | Tunable between DFS and Prim's via weight |
| 7 | Wilson's | `wilson` | Loop-erased random walk for uniform spanning tree |
| 8 | Aldous-Broder | `aldous_broder` | Random walk visiting every cell exactly once |
| 9 | Hunt and Kill | `hunt_and_kill` | Walk until stuck, then scan for unvisited neighbor |
| 10 | Pac-Man Maze | `pacman` | Bilaterally symmetric maze with central chamber |

### Room Placement (11)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 11 | BSP Tree | `bsp` | Binary space partition with rooms in leaves |
| 12 | Poisson Disk | `poisson` | Blue noise distributed room placement |
| 13 | Rooms and Mazes | `rooms_and_mazes` | Place rooms then fill gaps with maze corridors |
| 14 | Rogue Classic | `rogue` | Grid-based room layout inspired by original Rogue |
| 15 | Room Accretion | `accretion` | Grow dungeon by adding rooms one at a time |
| 16 | TinyKeep | `tinykeep` | Random rooms with physics-based separation |
| 17 | Feature Growing | `feature_growing` | Grow by attaching rooms and corridors to exits |
| 18 | BFS Room Expansion | `bfs_rooms` | Seed-based parallel flood fill room growing |
| 19 | Prefab Assembly | `prefab` | Place predefined room templates randomly |
| 20 | Gravity Collapse | `gravity_collapse` | Drop rooms from top, stack with gravity simulation |
| 21 | Tetromino Packing | `tetromino` | Pack tetris-shaped pieces into grid |

### Cave / Organic (15)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 22 | Cellular Automata | `cellular` | Conway-style birth/survival cave generation |
| 23 | Drunkard's Walk | `drunkard` | Random walker carving open space |
| 24 | Noise Caves | `noise_caves` | Value noise with octaves for smooth caves |
| 25 | DLA | `dla` | Diffusion-limited aggregation particle sticking |
| 26 | Marching Squares | `marching_squares` | Cellular automata with contour smoothing |
| 27 | Worm Tunnels | `worm` | Multiple directional worms carving passages |
| 28 | Erosion | `erosion` | Hydraulic erosion simulation on heightmap |
| 29 | Percolation | `percolation` | Random fill with connectivity phase transition |
| 30 | Ant Colony | `ant_colony` | Pheromone-guided ant swarm carving paths |
| 31 | Worley Noise | `worley` | Worley/cellular noise distance-based caves |
| 32 | Ridged Multifractal | `ridged` | Ridged multifractal noise with sharp ridges |
| 33 | Domain Warping | `domain_warp` | Noise with coordinate warping for organic shapes |
| 34 | Reaction-Diffusion | `reaction_diffusion` | Gray-Scott reaction-diffusion pattern formation |
| 35 | Slime Mold | `slime_mold` | Physarum-inspired slime mold agent simulation |
| 36 | Lightning | `lightning` | Lightning bolt branching path generation |

### Graph / Structure (10)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 37 | Graph Grammar | `graph_grammar` | Rule-based graph rewriting with locks and keys |
| 38 | Diablo | `diablo` | Linear spine with branching side paths and loops |
| 39 | Spelunky | `spelunky` | Grid cells with guaranteed solution path |
| 40 | Arena Chain | `arena_chain` | Large arenas connected by narrow corridors |
| 41 | Cyclic Dungeon | `cyclic_dungeon` | Circular connectivity with key-gate puzzles |
| 42 | Metroidvania | `metroidvania` | Multi-zone layout with ability-gated connections |
| 43 | Mission Graph | `mission_graph` | Mission-driven node graph with lock progression |
| 44 | Ring Dungeon | `ring` | Ring corridor with rooms and Dark Souls shortcuts |
| 45 | Binding of Isaac | `isaac` | Grid-based room layout inspired by Isaac |
| 46 | Dead Cells | `dead_cells` | Branching biome path layout inspired by Dead Cells |

### Space Filling (4)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 47 | Hilbert Curve | `hilbert` | Hilbert space-filling curve as corridor backbone |
| 48 | Gosper Curve | `gosper` | Flowsnake L-system curve on hex-like grid |
| 49 | Spiral | `spiral` | Archimedean spiral with rooms at intervals |
| 50 | Sierpinski Dungeon | `sierpinski` | Sierpinski carpet recursive subdivision |

### Terrain (5)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 51 | Diamond-Square | `diamond_square` | Fractal heightmap with configurable roughness |
| 52 | Midpoint Displacement | `midpoint_displacement` | Multi-layer midpoint displacement terrain |
| 53 | Island/Archipelago | `island` | Circular islands connected by bridges |
| 54 | River/Watershed | `river` | Heightmap with hydraulic river carving |
| 55 | Crystal Growth | `crystal` | Axis-aligned crystal seed growth |

### Pattern / Tiling (7)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 56 | WFC | `wfc` | Wave Function Collapse tile solver |
| 57 | Superimposition | `superimpose` | Overlapping random shape stamps |
| 58 | Herringbone Wang Tiles | `herringbone` | Herringbone pattern tile placement |
| 59 | L-System | `lsystem` | Lindenmayer system string rewriting paths |
| 60 | Hex Grid | `hex_grid` | Hexagonal cell grid with corridor connections |
| 61 | City Block | `city_block` | Street grid with buildings and dead ends |
| 62 | Penrose Tiling | `penrose` | Penrose-inspired aperiodic tiling dungeon |

### Hybrid (6)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 63 | Voronoi Region | `voronoi` | Voronoi cell regions with edge corridors |
| 64 | Radial | `radial` | Concentric rings with spokes and gaps |
| 65 | Space Colonization | `space_colonization` | Attractor-based branching path growth |
| 66 | Parametric Tunnelers | `tunneler` | Multiple autonomous tunneling agents |
| 67 | Platformer Level | `platformer` | Side-scrolling level with platforms and ladders |
| 68 | Delaunay + Gabriel | `delaunay` | Delaunay triangulation with Gabriel graph pruning |

### Hex (10)

| # | Name | Package | Grid | Description |
|---|------|---------|------|-------------|
| 69 | Hex Maze | `hex_maze` | HexGrid | DFS maze on hexagonal grid |
| 70 | Hex CA | `hex_ca` | HexGrid | Cellular automata on hex grid |
| 71 | Hex Voronoi | `hex_voronoi` | HexGrid | Voronoi regions on hex grid |
| 72 | Hex WFC | `hex_wfc` | HexGrid | Wave Function Collapse on hex grid |
| 73 | Hex Spiral | `hex_spiral` | HexGrid | Spiral path on hex grid |
| 74 | Hex Territory | `hex_territory` | HexGrid | Faction territory expansion on hex grid |
| 75 | Hex River Delta | `hex_river_delta` | HexGrid | Branching river channels from center |
| 76 | Hex Fortress | `hex_fortress` | HexGrid | Concentric hex walls with gates and towers |
| 77 | Hex Biome | `hex_biome` | HexGrid | Voronoi biome regions with roads |
| 78 | Hex Snowflake | `hex_snowflake` | HexGrid | Fractal snowflake crystal pattern |

### 3D Multi-Layer (10)

| # | Name | Package | Grid | Description |
|---|------|---------|------|-------------|
| 79 | Multi-Floor BSP | `multifloor_bsp` | Grid3D | BSP rooms on multiple floors with stairs |
| 80 | 3D Drunkard | `drunkard_3d` | Grid3D | Random walk across layers |
| 81 | 3D CA | `cellular_3d` | Grid3D | 3D cellular automata caves |
| 82 | Vertical Shaft | `vertical_shaft` | Grid3D | Vertical shafts connecting horizontal rooms |
| 83 | Layered Caves | `layered_caves` | Grid3D | Connected cave systems across layers |
| 84 | 3D Maze | `maze_3d` | Grid3D | Perfect maze with vertical passages |
| 85 | Underwater Base | `underwater_base` | Grid3D | Pressurized chambers with airlocks |
| 86 | Tree Roots | `tree_roots` | Grid3D | Organic branching roots descending through layers |
| 87 | Ant Nest 3D | `ant_nest_3d` | Grid3D | Ant colony with tunnels and chambers |
| 88 | Space Station | `space_station` | Grid3D | Ring habitat with central hub and spokes |

### Quarter View / Isometric (16)

| # | Name | Package | Grid | Description |
|---|------|---------|------|-------------|
| 89 | Isometric City | `iso_city` | HeightGrid | City blocks with variable building heights |
| 90 | Cliff Dungeon | `cliff_dungeon` | HeightGrid | Rooms carved into cliff faces |
| 91 | Layered Plateau | `layered_plateau` | HeightGrid | Eroded plateau terraces |
| 92 | Castle Generator | `castle_gen` | HeightGrid | Castle with towers, walls, and courtyards |
| 93 | Terraced Mines | `terraced_mines` | HeightGrid | Open-pit mine with ore veins |
| 94 | Floating Islands | `floating_islands` | HeightGrid | Sky islands connected by bridges |
| 95 | Ziggurat Temple | `ziggurat` | HeightGrid | Stepped pyramid with inner chambers |
| 96 | Canyon Ravine | `canyon` | HeightGrid | Deep canyon with bridges and caves |
| 97 | Harbor Town | `harbor_town` | HeightGrid | Coastal town with waterfront buildings |
| 98 | Tower Dungeon | `tower_dungeon` | HeightGrid | Vertical tower with stacked floors |
| 99 | Volcanic Crater | `volcanic_crater` | HeightGrid | Volcanic crater with lava vents |
| 100 | Amphitheater | `amphitheater` | HeightGrid | Tiered seating around central arena |
| 101 | Aqueduct | `aqueduct` | HeightGrid | Roman aqueduct with arched supports |
| 102 | Treehouse Village | `treehouse` | HeightGrid | Tree platforms connected by bridges |
| 103 | Mineshaft Elevator | `mineshaft` | HeightGrid | Central shaft with radiating tunnels |
| 104 | Glacier | `glacier` | HeightGrid | Ice field with crevasses and caves |

### Biome (6)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 105 | Continent | `continent` | Ocean, beach, forest, mountain concentric rings |
| 106 | Dungeon Ecosystem | `ecosystem` | Cave to underground river to lava biome transition |
| 107 | Seasonal Forest | `seasonal_forest` | North-to-south tree density gradient |
| 108 | Swamp Labyrinth | `swamp` | Intertwined water channels and land paths |
| 109 | Desert Oasis | `desert_oasis` | Desert with scattered oases and caravan routes |
| 110 | Tundra Outpost | `tundra` | Frozen outposts connected by underground tunnels |

### Puzzle / Progression (6)

| # | Name | Package | Description |
|---|------|---------|-------------|
| 111 | Sokoban Layout | `sokoban` | Push-block puzzle grid rooms |
| 112 | One-Way Gate | `oneway_gate` | Directed tree dungeon with one-way doors |
| 113 | Teleporter Maze | `teleporter` | Disconnected room clusters linked by teleporters |
| 114 | Lock Cascade | `lock_cascade` | Deep key-lock chain progression |
| 115 | Gravity Puzzle | `gravity_puzzle` | Split gravity zones with inverted platforms |
| 116 | Mirror Dungeon | `mirror_dungeon` | Symmetric dungeon with corrupted mirror half |

## Development

```bash
# Install MoonBit
curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash

# Check
moon check --deny-warn --target js

# Test
moon test --target js

# Build web playground
moon build --target js

# Serve locally
python3 -m http.server 8080
# Open http://localhost:8080
```

## License

Apache-2.0
