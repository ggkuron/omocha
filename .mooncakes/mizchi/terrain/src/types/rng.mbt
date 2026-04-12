///|
pub(all) struct RNG {
  mut state : UInt
}

///|
pub fn make_rng(seed : UInt) -> RNG {
  { state: if seed == 0U { 1U } else { seed } }
}

///|
pub fn RNG::next(self : RNG) -> UInt {
  // xorshift32
  let mut s = self.state
  s = s ^ (s << 13)
  s = s ^ (s >> 17)
  s = s ^ (s << 5)
  self.state = s
  s
}

///|
pub fn RNG::next_int(self : RNG, max : Int) -> Int {
  if max <= 0 {
    return 0
  }
  (self.next().reinterpret_as_int() & 0x7FFFFFFF) % max
}

///|
pub fn RNG::rand_range(self : RNG, min : Int, max : Int) -> Int {
  if min >= max {
    return min
  }
  min + self.next_int(max - min)
}

///|
pub fn RNG::rand_bool(self : RNG, probability : Double) -> Bool {
  let v = (self.next().reinterpret_as_int() & 0x7FFFFFFF).to_double() /
    2147483647.0
  v < probability
}

///|
pub fn[T] RNG::rand_pick(self : RNG, arr : Array[T]) -> T {
  arr[self.next_int(arr.length())]
}

///|
pub fn[T] RNG::rand_shuffle(self : RNG, arr : Array[T]) -> Unit {
  for i = arr.length() - 1; i > 0; {
    let j = self.next_int(i + 1)
    arr.swap(i, j)
    continue i - 1
  }
}

///|
pub fn RNG::rand_direction(self : RNG) -> Point {
  let dirs : Array[Point] = [
    { x: -1, y: 0 },
    { x: 1, y: 0 },
    { x: 0, y: -1 },
    { x: 0, y: 1 },
  ]
  self.rand_pick(dirs)
}
