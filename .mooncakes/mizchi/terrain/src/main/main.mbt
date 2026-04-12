///|
fn run_algorithm(name : String, grid : @types.Grid2D, room_count : Int) -> Unit {
  println("=== " + name + " ===")
  println(grid.to_ascii())
  let metrics = @types.compute_metrics(grid, room_count)
  println(
    "  open_ratio: " + metrics.open_ratio.to_string() +
    "  rooms: " + metrics.room_count.to_string() +
    "  connected: " + metrics.connectivity.to_string() +
    "  largest: " + metrics.largest_component.to_string() +
    "  dead_ends: " + metrics.dead_end_count.to_string(),
  )
  println("")
}

///|
fn main {
  let seed = 42U
  println("terrain - dungeon generation algorithms (seed=" + seed.to_string() + ")")
  println("")
  // 1. Drunkard's Walk
  let rng = @types.make_rng(seed)
  let dr = @drunkard.generate(@drunkard.DrunkardConfig::default(), rng)
  run_algorithm("Drunkard's Walk", dr.grid, 0)
  // 2. Cellular Automata
  let rng = @types.make_rng(seed)
  let ca = @cellular.generate(@cellular.CellularConfig::default(), rng)
  run_algorithm("Cellular Automata", ca.grid, 0)
  // 3. BSP Tree
  let rng = @types.make_rng(seed)
  let bsp = @bsp.generate(@bsp.BspConfig::default(), rng)
  run_algorithm("BSP Tree", bsp.grid, bsp.rooms.length())
  // 4. Poisson Disk
  let rng = @types.make_rng(seed)
  let ps = @poisson.generate(@poisson.PoissonConfig::default(), rng)
  run_algorithm("Poisson Disk", ps.grid, ps.rooms.length())
  // 5. Graph Grammar
  let rng = @types.make_rng(seed)
  let gg = @graph_grammar.generate(@graph_grammar.GraphGrammarConfig::default(), rng)
  run_algorithm("Graph Grammar", gg.grid, gg.rooms.length())
  // 6. WFC
  let rng = @types.make_rng(seed)
  let wfc = @wfc.generate(@wfc.WfcConfig::default(), rng)
  run_algorithm("WFC (success=" + wfc.success.to_string() + ")", wfc.grid, 0)
  // 7. Diablo
  let rng = @types.make_rng(seed)
  let diablo = @diablo.generate(@diablo.DiabloConfig::default(), rng)
  run_algorithm("Diablo", diablo.grid, diablo.rooms.length())
  // 8. Spelunky
  let rng = @types.make_rng(seed)
  let spelunky = @spelunky.generate(@spelunky.SpelunkyConfig::default(), rng)
  run_algorithm("Spelunky", spelunky.grid, spelunky.rooms.length())
  // 9. Arena Chain
  let rng = @types.make_rng(seed)
  let arena = @arena_chain.generate(@arena_chain.ArenaChainConfig::default(), rng)
  run_algorithm("Arena Chain", arena.grid, arena.rooms.length())
  // 10. Voronoi Region
  let rng = @types.make_rng(seed)
  let voronoi = @voronoi.generate(@voronoi.VoronoiConfig::default(), rng)
  run_algorithm("Voronoi Region", voronoi.grid, voronoi.rooms.length())
  // 11. Maze
  let rng = @types.make_rng(seed)
  let mz = @maze.generate(@maze.MazeConfig::default(), rng)
  run_algorithm("Maze (DFS)", mz.grid, mz.rooms.length())
  // 12. Rooms and Mazes
  let rng = @types.make_rng(seed)
  let rm = @rooms_and_mazes.generate(
    @rooms_and_mazes.RoomsAndMazesConfig::default(),
    rng,
  )
  run_algorithm("Rooms and Mazes", rm.grid, rm.rooms.length())
  // 13. Radial
  let rng = @types.make_rng(seed)
  let rad = @radial.generate(@radial.RadialConfig::default(), rng)
  run_algorithm("Radial", rad.grid, rad.rooms.length())
  // 14. DLA
  let rng = @types.make_rng(seed)
  let dla = @dla.generate(@dla.DlaConfig::default(), rng)
  run_algorithm("DLA", dla.grid, dla.particles)
  // 15. Worm Tunnels
  let rng = @types.make_rng(seed)
  let worm = @worm.generate(@worm.WormConfig::default(), rng)
  run_algorithm("Worm Tunnels", worm.grid, worm.rooms.length())
  // 16. Rogue Classic
  let rng = @types.make_rng(seed)
  let rogue = @rogue.generate(@rogue.RogueConfig::default(), rng)
  run_algorithm("Rogue Classic", rogue.grid, rogue.rooms.length())
  // 17. Room Accretion
  let rng = @types.make_rng(seed)
  let acc = @accretion.generate(@accretion.AccretionConfig::default(), rng)
  run_algorithm("Room Accretion", acc.grid, acc.rooms.length())
  // 18. TinyKeep
  let rng = @types.make_rng(seed)
  let tk = @tinykeep.generate(@tinykeep.TinyKeepConfig::default(), rng)
  run_algorithm("TinyKeep", tk.grid, tk.rooms.length())
  // 19. Feature Growing
  let rng = @types.make_rng(seed)
  let fg = @feature_growing.generate(
    @feature_growing.FeatureGrowingConfig::default(),
    rng,
  )
  run_algorithm("Feature Growing", fg.grid, fg.rooms.length())
  // 20. Noise Caves
  let rng = @types.make_rng(seed)
  let nc = @noise_caves.generate(@noise_caves.NoiseCavesConfig::default(), rng)
  run_algorithm("Noise Caves", nc.grid, nc.open_cells)
  // 21. Superimposition
  let rng = @types.make_rng(seed)
  let si = @superimpose.generate(@superimpose.SuperimposeConfig::default(), rng)
  run_algorithm("Superimposition", si.grid, si.rooms.length())
  // 22. Eller's Maze
  let rng = @types.make_rng(seed)
  let el = @eller.generate(@eller.EllerConfig::default(), rng)
  run_algorithm("Eller's Maze", el.grid, el.rooms.length())
  // 23. Parametric Tunnelers
  let rng = @types.make_rng(seed)
  let tn = @tunneler.generate(@tunneler.TunnelerConfig::default(), rng)
  run_algorithm("Parametric Tunnelers", tn.grid, tn.rooms.length())
  // 24. BFS Room Expansion
  let rng = @types.make_rng(seed)
  let bf = @bfs_rooms.generate(@bfs_rooms.BfsRoomsConfig::default(), rng)
  run_algorithm("BFS Room Expansion", bf.grid, bf.rooms.length())
  // 25. Kruskal's Maze
  let rng = @types.make_rng(seed)
  let kr = @kruskal.generate(@kruskal.KruskalConfig::default(), rng)
  run_algorithm("Kruskal's Maze", kr.grid, kr.rooms.length())
  // 26. Recursive Division
  let rng = @types.make_rng(seed)
  let rd = @recursive_division.generate(
    @recursive_division.RecursiveDivisionConfig::default(),
    rng,
  )
  run_algorithm("Recursive Division", rd.grid, rd.rooms.length())
  // 27. Binary Tree Maze
  let rng = @types.make_rng(seed)
  let bt = @binary_tree.generate(@binary_tree.BinaryTreeConfig::default(), rng)
  run_algorithm("Binary Tree Maze", bt.grid, bt.rooms.length())
  // 28. Sidewinder Maze
  let rng = @types.make_rng(seed)
  let sw = @sidewinder.generate(@sidewinder.SidewinderConfig::default(), rng)
  run_algorithm("Sidewinder Maze", sw.grid, sw.rooms.length())
  // 29. Diamond-Square
  let rng = @types.make_rng(seed)
  let ds = @diamond_square.generate(
    @diamond_square.DiamondSquareConfig::default(),
    rng,
  )
  run_algorithm("Diamond-Square", ds.grid, ds.rooms.length())
  // 30. Space Colonization
  let rng = @types.make_rng(seed)
  let sc = @space_colonization.generate(
    @space_colonization.SpaceColonizationConfig::default(),
    rng,
  )
  run_algorithm("Space Colonization", sc.grid, sc.rooms.length())
  // 31. Hilbert Curve
  let rng = @types.make_rng(seed)
  let hb = @hilbert.generate(@hilbert.HilbertConfig::default(), rng)
  run_algorithm("Hilbert Curve", hb.grid, hb.rooms.length())
  // 32. Prefab Assembly
  let rng = @types.make_rng(seed)
  let pf = @prefab.generate(@prefab.PrefabConfig::default(), rng)
  run_algorithm("Prefab Assembly", pf.grid, pf.rooms.length())
  // 33. Growing Tree
  let rng = @types.make_rng(seed)
  let gt = @growing_tree.generate(@growing_tree.GrowingTreeConfig::default(), rng)
  run_algorithm("Growing Tree", gt.grid, gt.rooms.length())
  // 34. Wilson's
  let rng = @types.make_rng(seed)
  let wl = @wilson.generate(@wilson.WilsonConfig::default(), rng)
  run_algorithm("Wilson's", wl.grid, wl.rooms.length())
  // 35. Aldous-Broder
  let rng = @types.make_rng(seed)
  let ab = @aldous_broder.generate(@aldous_broder.AldousBroderConfig::default(), rng)
  run_algorithm("Aldous-Broder", ab.grid, ab.rooms.length())
  // 36. Hunt and Kill
  let rng = @types.make_rng(seed)
  let hk = @hunt_and_kill.generate(@hunt_and_kill.HuntAndKillConfig::default(), rng)
  run_algorithm("Hunt and Kill", hk.grid, hk.rooms.length())
  // 37. Erosion
  let rng = @types.make_rng(seed)
  let er = @erosion.generate(@erosion.ErosionConfig::default(), rng)
  run_algorithm("Erosion", er.grid, er.rivers)
  // 38. Midpoint Displacement
  let rng = @types.make_rng(seed)
  let md = @midpoint_displacement.generate(@midpoint_displacement.MidpointDisplacementConfig::default(), rng)
  run_algorithm("Midpoint Displacement", md.grid, md.open_cells)
  // 39. Island/Archipelago
  let rng = @types.make_rng(seed)
  let is_ = @island.generate(@island.IslandConfig::default(), rng)
  run_algorithm("Island/Archipelago", is_.grid, is_.rooms.length())
  // 40. Cyclic Dungeon
  let rng = @types.make_rng(seed)
  let cd = @cyclic_dungeon.generate(@cyclic_dungeon.CyclicDungeonConfig::default(), rng)
  run_algorithm("Cyclic Dungeon", cd.grid, cd.rooms.length())
  // 41. Metroidvania
  let rng = @types.make_rng(seed)
  let mv = @metroidvania.generate(@metroidvania.MetroidvaniaConfig::default(), rng)
  run_algorithm("Metroidvania", mv.grid, mv.rooms.length())
  // 42. Mission Graph
  let rng = @types.make_rng(seed)
  let mg = @mission_graph.generate(@mission_graph.MissionGraphConfig::default(), rng)
  run_algorithm("Mission Graph", mg.grid, mg.rooms.length())
  // 43. Herringbone Wang Tiles
  let rng = @types.make_rng(seed)
  let hb2 = @herringbone.generate(@herringbone.HerringboneConfig::default(), rng)
  run_algorithm("Herringbone Wang Tiles", hb2.grid, hb2.rooms.length())
  // 44. L-System
  let rng = @types.make_rng(seed)
  let ls = @lsystem.generate(@lsystem.LsystemConfig::default(), rng)
  run_algorithm("L-System", ls.grid, ls.rooms.length())
  // 45. Marching Squares
  let rng = @types.make_rng(seed)
  let ms = @marching_squares.generate(@marching_squares.MarchingSquaresConfig::default(), rng)
  run_algorithm("Marching Squares", ms.grid, ms.open_cells)
  // 46. Ant Colony
  let rng = @types.make_rng(seed)
  let ac = @ant_colony.generate(@ant_colony.AntColonyConfig::default(), rng)
  run_algorithm("Ant Colony", ac.grid, ac.rooms.length())
  // 47. Percolation
  let rng = @types.make_rng(seed)
  let pc = @percolation.generate(@percolation.PercolationConfig::default(), rng)
  run_algorithm("Percolation", pc.grid, pc.open_cells)
  // 48. Gravity Collapse
  let rng = @types.make_rng(seed)
  let gc = @gravity_collapse.generate(@gravity_collapse.GravityCollapseConfig::default(), rng)
  run_algorithm("Gravity Collapse", gc.grid, gc.rooms.length())
  // 49. Sierpinski Dungeon
  let rng = @types.make_rng(seed)
  let sp = @sierpinski.generate(@sierpinski.SierpinskiConfig::default(), rng)
  run_algorithm("Sierpinski Dungeon", sp.grid, sp.rooms.length())
  // 50. Spiral
  let rng = @types.make_rng(seed)
  let sr = @spiral.generate(@spiral.SpiralConfig::default(), rng)
  run_algorithm("Spiral", sr.grid, sr.rooms.length())
  // 51. Gosper Curve
  let rng = @types.make_rng(seed)
  let gp = @gosper.generate(@gosper.GosperConfig::default(), rng)
  run_algorithm("Gosper Curve", gp.grid, gp.rooms.length())
  // 52. Platformer Level
  let rng = @types.make_rng(seed)
  let pl = @platformer.generate(@platformer.PlatformerConfig::default(), rng)
  run_algorithm("Platformer Level", pl.grid, pl.rooms.length())
  // 53. Ring Dungeon
  let rng = @types.make_rng(seed)
  let rn = @ring.generate(@ring.RingConfig::default(), rng)
  run_algorithm("Ring Dungeon", rn.grid, rn.rooms.length())
  // 54. Pac-Man Maze
  let rng = @types.make_rng(seed)
  let pm = @pacman.generate(@pacman.PacmanConfig::default(), rng)
  run_algorithm("Pac-Man Maze", pm.grid, pm.rooms.length())
  // 55. Tetromino Packing
  let rng = @types.make_rng(seed)
  let tt = @tetromino.generate(@tetromino.TetrominoConfig::default(), rng)
  run_algorithm("Tetromino Packing", tt.grid, tt.rooms.length())
  // 56. Hex Grid
  let rng = @types.make_rng(seed)
  let hx = @hex_grid.generate(@hex_grid.HexGridConfig::default(), rng)
  run_algorithm("Hex Grid", hx.grid, hx.rooms.length())
  // 57. City Block
  let rng = @types.make_rng(seed)
  let cb = @city_block.generate(@city_block.CityBlockConfig::default(), rng)
  run_algorithm("City Block", cb.grid, cb.rooms.length())
  // 58. Worley Noise
  let rng = @types.make_rng(seed)
  let wn = @worley.generate(@worley.WorleyConfig::default(), rng)
  run_algorithm("Worley Noise", wn.grid, wn.open_cells)
  // 59. Ridged Multifractal
  let rng = @types.make_rng(seed)
  let rf = @ridged.generate(@ridged.RidgedConfig::default(), rng)
  run_algorithm("Ridged Multifractal", rf.grid, rf.open_cells)
  // 60. Domain Warping
  let rng = @types.make_rng(seed)
  let dw = @domain_warp.generate(@domain_warp.DomainWarpConfig::default(), rng)
  run_algorithm("Domain Warping", dw.grid, dw.open_cells)
  // 61. Reaction-Diffusion
  let rng = @types.make_rng(seed)
  let rd2 = @reaction_diffusion.generate(@reaction_diffusion.ReactionDiffusionConfig::default(), rng)
  run_algorithm("Reaction-Diffusion", rd2.grid, rd2.open_cells)
  // 62. River/Watershed
  let rng = @types.make_rng(seed)
  let rv = @river.generate(@river.RiverConfig::default(), rng)
  run_algorithm("River/Watershed", rv.grid, rv.rivers)
  // 63. Crystal Growth
  let rng = @types.make_rng(seed)
  let cr = @crystal.generate(@crystal.CrystalConfig::default(), rng)
  run_algorithm("Crystal Growth", cr.grid, cr.rooms.length())
  // 64. Slime Mold
  let rng = @types.make_rng(seed)
  let sm = @slime_mold.generate(@slime_mold.SlimeMoldConfig::default(), rng)
  run_algorithm("Slime Mold", sm.grid, sm.rooms.length())
  // 65. Lightning
  let rng = @types.make_rng(seed)
  let lg = @lightning.generate(@lightning.LightningConfig::default(), rng)
  run_algorithm("Lightning", lg.grid, lg.rooms.length())
  // 66. Binding of Isaac
  let rng = @types.make_rng(seed)
  let bi = @isaac.generate(@isaac.IsaacConfig::default(), rng)
  run_algorithm("Binding of Isaac", bi.grid, bi.rooms.length())
  // 67. Dead Cells
  let rng = @types.make_rng(seed)
  let dc = @dead_cells.generate(@dead_cells.DeadCellsConfig::default(), rng)
  run_algorithm("Dead Cells", dc.grid, dc.rooms.length())
  // 68. Penrose Tiling
  let rng = @types.make_rng(seed)
  let pt = @penrose.generate(@penrose.PenroseConfig::default(), rng)
  run_algorithm("Penrose Tiling", pt.grid, pt.rooms.length())
  // 69. Delaunay + Gabriel
  let rng = @types.make_rng(seed)
  let dl = @delaunay.generate(@delaunay.DelaunayConfig::default(), rng)
  run_algorithm("Delaunay + Gabriel", dl.grid, dl.rooms.length())
  // === Hex ===
  // 70. Hex Maze
  let rng = @types.make_rng(seed)
  let hm = @hex_maze.generate(@hex_maze.HexMazeConfig::default(), rng)
  run_algorithm("Hex Maze", hm.grid.to_grid2d(2), hm.rooms.length())
  // 71. Hex CA
  let rng = @types.make_rng(seed)
  let hca = @hex_ca.generate(@hex_ca.HexCaConfig::default(), rng)
  run_algorithm("Hex CA", hca.grid.to_grid2d(2), hca.open_cells)
  // 72. Hex Voronoi
  let rng = @types.make_rng(seed)
  let hv = @hex_voronoi.generate(@hex_voronoi.HexVoronoiConfig::default(), rng)
  run_algorithm("Hex Voronoi", hv.grid.to_grid2d(2), hv.rooms.length())
  // 73. Hex WFC
  let rng = @types.make_rng(seed)
  let hw = @hex_wfc.generate(@hex_wfc.HexWfcConfig::default(), rng)
  run_algorithm("Hex WFC", hw.grid.to_grid2d(2), 0)
  // 74. Hex Spiral
  let rng = @types.make_rng(seed)
  let hs = @hex_spiral.generate(@hex_spiral.HexSpiralConfig::default(), rng)
  run_algorithm("Hex Spiral", hs.grid.to_grid2d(2), hs.rooms.length())
  // 75. Hex Territory
  let rng = @types.make_rng(seed)
  let ht = @hex_territory.generate(@hex_territory.HexTerritoryConfig::default(), rng)
  run_algorithm("Hex Territory", ht.grid.to_grid2d(2), ht.rooms.length())
  // === 3D ===
  // 76. Multi-Floor BSP
  let rng = @types.make_rng(seed)
  let mb = @multifloor_bsp.generate(@multifloor_bsp.MultifloorBspConfig::default(), rng)
  run_algorithm("Multi-Floor BSP", mb.grid.get_layer(0), mb.rooms.length())
  // 77. 3D Drunkard
  let rng = @types.make_rng(seed)
  let d3 = @drunkard_3d.generate(@drunkard_3d.Drunkard3dConfig::default(), rng)
  run_algorithm("3D Drunkard", d3.grid.get_layer(0), d3.steps_taken)
  // 78. 3D CA
  let rng = @types.make_rng(seed)
  let c3 = @cellular_3d.generate(@cellular_3d.Cellular3dConfig::default(), rng)
  run_algorithm("3D CA", c3.grid.get_layer(0), c3.open_cells)
  // 79. Vertical Shaft
  let rng = @types.make_rng(seed)
  let vs = @vertical_shaft.generate(@vertical_shaft.VerticalShaftConfig::default(), rng)
  run_algorithm("Vertical Shaft", vs.grid.get_layer(0), vs.rooms.length())
  // 80. Layered Caves
  let rng = @types.make_rng(seed)
  let lc = @layered_caves.generate(@layered_caves.LayeredCavesConfig::default(), rng)
  run_algorithm("Layered Caves", lc.grid.get_layer(0), lc.open_cells)
  // 81. 3D Maze
  let rng = @types.make_rng(seed)
  let m3 = @maze_3d.generate(@maze_3d.Maze3dConfig::default(), rng)
  run_algorithm("3D Maze", m3.grid.get_layer(0), m3.rooms.length())
  // === Quarter View ===
  // 82. Isometric City
  let rng = @types.make_rng(seed)
  let ic = @iso_city.generate(@iso_city.IsoCityConfig::default(), rng)
  run_algorithm("Isometric City", ic.grid.to_grid2d(), ic.rooms.length())
  // 83. Cliff Dungeon
  let rng = @types.make_rng(seed)
  let cld = @cliff_dungeon.generate(@cliff_dungeon.CliffDungeonConfig::default(), rng)
  run_algorithm("Cliff Dungeon", cld.grid.to_grid2d(), cld.rooms.length())
  // 84. Layered Plateau
  let rng = @types.make_rng(seed)
  let lp = @layered_plateau.generate(@layered_plateau.LayeredPlateauConfig::default(), rng)
  run_algorithm("Layered Plateau", lp.grid.to_grid2d(), lp.rooms.length())
  // 85. Castle Generator
  let rng = @types.make_rng(seed)
  let cg = @castle_gen.generate(@castle_gen.CastleGenConfig::default(), rng)
  run_algorithm("Castle Generator", cg.grid.to_grid2d(), cg.rooms.length())
  // 86. Terraced Mines
  let rng = @types.make_rng(seed)
  let tm = @terraced_mines.generate(@terraced_mines.TerracedMinesConfig::default(), rng)
  run_algorithm("Terraced Mines", tm.grid.to_grid2d(), tm.rooms.length())
  // 87. Floating Islands
  let rng = @types.make_rng(seed)
  let fi = @floating_islands.generate(@floating_islands.FloatingIslandsConfig::default(), rng)
  run_algorithm("Floating Islands", fi.grid.to_grid2d(), fi.rooms.length())
  // 88. Ziggurat Temple
  let rng = @types.make_rng(seed)
  let zg = @ziggurat.generate(@ziggurat.ZigguratConfig::default(), rng)
  run_algorithm("Ziggurat Temple", zg.grid.to_grid2d(), zg.rooms.length())
  // 89. Canyon Ravine
  let rng = @types.make_rng(seed)
  let cn = @canyon.generate(@canyon.CanyonConfig::default(), rng)
  run_algorithm("Canyon Ravine", cn.grid.to_grid2d(), cn.rooms.length())
  // 90. Harbor Town
  let rng = @types.make_rng(seed)
  let ht2 = @harbor_town.generate(@harbor_town.HarborTownConfig::default(), rng)
  run_algorithm("Harbor Town", ht2.grid.to_grid2d(), ht2.rooms.length())
  // 91. Tower Dungeon
  let rng = @types.make_rng(seed)
  let td = @tower_dungeon.generate(@tower_dungeon.TowerDungeonConfig::default(), rng)
  run_algorithm("Tower Dungeon", td.grid.to_grid2d(), td.rooms.length())
  // 92. Volcanic Crater
  let rng = @types.make_rng(seed)
  let vc = @volcanic_crater.generate(@volcanic_crater.VolcanicCraterConfig::default(), rng)
  run_algorithm("Volcanic Crater", vc.grid.to_grid2d(), vc.rooms.length())
  // 93. Amphitheater
  let rng = @types.make_rng(seed)
  let am = @amphitheater.generate(@amphitheater.AmphitheaterConfig::default(), rng)
  run_algorithm("Amphitheater", am.grid.to_grid2d(), am.rooms.length())
  // === Biome ===
  // 94. Continent
  let rng = @types.make_rng(seed)
  let cont = @continent.generate(@continent.ContinentConfig::default(), rng)
  run_algorithm("Continent", cont.grid, cont.rooms.length())
  // 95. Ecosystem
  let rng = @types.make_rng(seed)
  let eco = @ecosystem.generate(@ecosystem.EcosystemConfig::default(), rng)
  run_algorithm("Ecosystem", eco.grid, eco.rooms.length())
  // 96. Seasonal Forest
  let rng = @types.make_rng(seed)
  let sf = @seasonal_forest.generate(@seasonal_forest.SeasonalForestConfig::default(), rng)
  run_algorithm("Seasonal Forest", sf.grid, sf.rooms.length())
  // 97. Swamp
  let rng = @types.make_rng(seed)
  let swp = @swamp.generate(@swamp.SwampConfig::default(), rng)
  run_algorithm("Swamp", swp.grid, swp.rooms.length())
  // 98. Desert Oasis
  let rng = @types.make_rng(seed)
  let doa = @desert_oasis.generate(@desert_oasis.DesertOasisConfig::default(), rng)
  run_algorithm("Desert Oasis", doa.grid, doa.rooms.length())
  // 99. Tundra
  let rng = @types.make_rng(seed)
  let tun = @tundra.generate(@tundra.TundraConfig::default(), rng)
  run_algorithm("Tundra", tun.grid, tun.rooms.length())
  // === Puzzle ===
  // 100. Sokoban
  let rng = @types.make_rng(seed)
  let sok = @sokoban.generate(@sokoban.SokobanConfig::default(), rng)
  run_algorithm("Sokoban", sok.grid, sok.rooms.length())
  // 101. One-Way Gate
  let rng = @types.make_rng(seed)
  let owg = @oneway_gate.generate(@oneway_gate.OnewayGateConfig::default(), rng)
  run_algorithm("One-Way Gate", owg.grid, owg.rooms.length())
  // 102. Teleporter Maze
  let rng = @types.make_rng(seed)
  let tel = @teleporter.generate(@teleporter.TeleporterConfig::default(), rng)
  run_algorithm("Teleporter Maze", tel.grid, tel.rooms.length())
  // 103. Lock Cascade
  let rng = @types.make_rng(seed)
  let lck = @lock_cascade.generate(@lock_cascade.LockCascadeConfig::default(), rng)
  run_algorithm("Lock Cascade", lck.grid, lck.rooms.length())
  // 104. Gravity Puzzle
  let rng = @types.make_rng(seed)
  let gpz = @gravity_puzzle.generate(@gravity_puzzle.GravityPuzzleConfig::default(), rng)
  run_algorithm("Gravity Puzzle", gpz.grid, gpz.rooms.length())
  // 105. Mirror Dungeon
  let rng = @types.make_rng(seed)
  let mir = @mirror_dungeon.generate(@mirror_dungeon.MirrorDungeonConfig::default(), rng)
  run_algorithm("Mirror Dungeon", mir.grid, mir.rooms.length())
  // === Hex Add ===
  // 106. Hex River Delta
  let rng = @types.make_rng(seed)
  let hrd = @hex_river_delta.generate(@hex_river_delta.HexRiverDeltaConfig::default(), rng)
  run_algorithm("Hex River Delta", hrd.grid.to_grid2d(2), hrd.rooms.length())
  // 107. Hex Fortress
  let rng = @types.make_rng(seed)
  let hft = @hex_fortress.generate(@hex_fortress.HexFortressConfig::default(), rng)
  run_algorithm("Hex Fortress", hft.grid.to_grid2d(2), hft.rooms.length())
  // 108. Hex Biome
  let rng = @types.make_rng(seed)
  let hbi = @hex_biome.generate(@hex_biome.HexBiomeConfig::default(), rng)
  run_algorithm("Hex Biome", hbi.grid.to_grid2d(2), hbi.rooms.length())
  // 109. Hex Snowflake
  let rng = @types.make_rng(seed)
  let hsf = @hex_snowflake.generate(@hex_snowflake.HexSnowflakeConfig::default(), rng)
  run_algorithm("Hex Snowflake", hsf.grid.to_grid2d(2), hsf.rooms.length())
  // === 3D Add ===
  // 110. Underwater Base
  let rng = @types.make_rng(seed)
  let ub = @underwater_base.generate(@underwater_base.UnderwaterBaseConfig::default(), rng)
  run_algorithm("Underwater Base", ub.grid.get_layer(0), ub.rooms.length())
  // 111. Tree Roots
  let rng = @types.make_rng(seed)
  let tr = @tree_roots.generate(@tree_roots.TreeRootsConfig::default(), rng)
  run_algorithm("Tree Roots", tr.grid.get_layer(0), tr.rooms.length())
  // 112. Ant Nest 3D
  let rng = @types.make_rng(seed)
  let an = @ant_nest_3d.generate(@ant_nest_3d.AntNest3dConfig::default(), rng)
  run_algorithm("Ant Nest 3D", an.grid.get_layer(0), an.rooms.length())
  // 113. Space Station
  let rng = @types.make_rng(seed)
  let ss = @space_station.generate(@space_station.SpaceStationConfig::default(), rng)
  run_algorithm("Space Station", ss.grid.get_layer(0), ss.rooms.length())
  // === Quarter View Add ===
  // 114. Aqueduct
  let rng = @types.make_rng(seed)
  let aq = @aqueduct.generate(@aqueduct.AqueductConfig::default(), rng)
  run_algorithm("Aqueduct", aq.grid.to_grid2d(), aq.rooms.length())
  // 115. Treehouse Village
  let rng = @types.make_rng(seed)
  let th = @treehouse.generate(@treehouse.TreehouseConfig::default(), rng)
  run_algorithm("Treehouse Village", th.grid.to_grid2d(), th.rooms.length())
  // 116. Mineshaft Elevator
  let rng = @types.make_rng(seed)
  let msh = @mineshaft.generate(@mineshaft.MineshaftConfig::default(), rng)
  run_algorithm("Mineshaft Elevator", msh.grid.to_grid2d(), msh.rooms.length())
  // 117. Glacier
  let rng = @types.make_rng(seed)
  let gl = @glacier.generate(@glacier.GlacierConfig::default(), rng)
  run_algorithm("Glacier", gl.grid.to_grid2d(), gl.rooms.length())
}
