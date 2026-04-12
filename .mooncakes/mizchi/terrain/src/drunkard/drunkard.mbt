///|
pub(all) struct DrunkardConfig {
  width : Int
  height : Int
  fill_ratio : Double
  max_steps : Int
}

///|
pub fn DrunkardConfig::default() -> DrunkardConfig {
  { width: 40, height: 20, fill_ratio: 0.4, max_steps: 5000 }
}

///|
pub(all) struct DrunkardResult {
  grid : @types.Grid2D
  steps_taken : Int
}

///|
pub fn generate(config : DrunkardConfig, rng : @types.RNG) -> DrunkardResult {
  let grid = @types.Grid2D::make(config.width, config.height, @types.Wall)
  let target_floors = (config.width.to_double() * config.height.to_double() *
  config.fill_ratio).to_int()
  let mut x = config.width / 2
  let mut y = config.height / 2
  let mut floors = 0
  let mut steps = 0
  grid.set(x, y, @types.Floor)
  floors += 1
  while floors < target_floors && steps < config.max_steps {
    let dir = rng.rand_direction()
    let nx = x + dir.x
    let ny = y + dir.y
    if nx > 0 && nx < config.width - 1 && ny > 0 && ny < config.height - 1 {
      x = nx
      y = ny
      if grid.get(x, y) == @types.Wall {
        grid.set(x, y, @types.Floor)
        floors += 1
      }
    }
    steps += 1
  }
  { grid, steps_taken: steps }
}
