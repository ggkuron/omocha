///|
test "Grid2D basic operations" {
  let grid = Grid2D::make(5, 5, Wall)
  grid.set(2, 2, Floor)
  assert_eq(grid.get(2, 2), Floor)
  assert_eq(grid.get(0, 0), Wall)
  assert_true(grid.in_bounds(4, 4))
  assert_true(not(grid.in_bounds(5, 5)))
}

///|
test "Grid2D fill_rect" {
  let grid = Grid2D::make(5, 5, Wall)
  grid.fill_rect({ x: 1, y: 1, w: 3, h: 3 }, Floor)
  let expected =
    #|#####
    #|#...#
    #|#...#
    #|#...#
    #|#####
  inspect(grid.to_ascii(), content=expected)
}

///|
test "Grid2D carve_line" {
  let grid = Grid2D::make(5, 5, Wall)
  grid.carve_line({ x: 0, y: 2 }, { x: 4, y: 2 }, Floor)
  let expected =
    #|#####
    #|#####
    #|.....
    #|#####
    #|#####
  inspect(grid.to_ascii(), content=expected)
}

///|
test "Grid2D count_neighbors" {
  let grid = Grid2D::make(3, 3, Wall)
  grid.set(0, 0, Floor)
  grid.set(1, 0, Floor)
  grid.set(0, 1, Floor)
  assert_eq(grid.count_neighbors(1, 1, Floor), 3)
}

///|
test "RNG deterministic" {
  let rng1 = make_rng(42U)
  let rng2 = make_rng(42U)
  let vals1 : Array[Int] = []
  let vals2 : Array[Int] = []
  for _ in 0..<10 {
    vals1.push(rng1.next_int(100))
    vals2.push(rng2.next_int(100))
  }
  assert_eq(vals1, vals2)
}

///|
test "RNG rand_range" {
  let rng = make_rng(123U)
  for _ in 0..<100 {
    let v = rng.rand_range(5, 10)
    assert_true(v >= 5 && v < 10)
  }
}

///|
test "Rect operations" {
  let r : Rect = { x: 1, y: 1, w: 4, h: 4 }
  assert_eq(r.center(), { x: 3, y: 3 })
  assert_true(r.contains({ x: 2, y: 2 }))
  assert_true(not(r.contains({ x: 0, y: 0 })))
  let r2 : Rect = { x: 3, y: 3, w: 4, h: 4 }
  assert_true(r.intersects(r2))
  let r3 : Rect = { x: 10, y: 10, w: 2, h: 2 }
  assert_true(not(r.intersects(r3)))
}

///|
test "flood_fill" {
  let grid = Grid2D::make(5, 5, Wall)
  grid.fill_rect({ x: 1, y: 1, w: 3, h: 3 }, Floor)
  let filled = flood_fill(grid, 2, 2)
  assert_eq(filled.length(), 9)
}

///|
test "compute_metrics" {
  let grid = Grid2D::make(5, 5, Wall)
  grid.fill_rect({ x: 1, y: 1, w: 3, h: 3 }, Floor)
  let m = compute_metrics(grid, 1)
  assert_true(m.connectivity)
  assert_eq(m.largest_component, 9)
  assert_eq(m.room_count, 1)
  assert_true(m.open_ratio > 0.3)
}
