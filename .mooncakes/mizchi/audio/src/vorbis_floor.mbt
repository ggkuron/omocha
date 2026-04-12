///|
/// Decode floor type 1 for a single channel.
/// Returns the floor curve as an array of float values, or None if "unused".
fn decode_floor1(
  br : BitReader,
  floor : VorbisFloorConfig,
  codebooks : Array[VorbisCodebook],
  n : Int,
) -> Array[Float]? raise AudioError {
  let nonzero = read_bit(br)
  if not(nonzero) {
    return None
  }
  let range_vals = [256, 128, 86, 64]
  let range = range_vals[floor.multiplier - 1]
  let y_list : Array[Int] = Array::make(floor.x_list.length(), 0)
  // Read floor1 Y values
  y_list[0] = read_bits(br, ilog(range - 1))
  y_list[1] = read_bits(br, ilog(range - 1))
  let mut offset = 2
  for i in 0..<floor.partitions {
    let cls = floor.partition_class[i]
    let dim = floor.class_dimensions[cls]
    let sub = floor.class_subclasses[cls]
    let mut csub = 0
    if sub > 0 {
      let master = floor.class_masterbooks[cls]
      if master < codebooks.length() {
        csub = codebook_decode(codebooks[master], br)
      }
    }
    for j in 0..<dim {
      let book_idx = if sub > 0 {
        floor.subclass_books[cls][(csub >> (j * sub)) & ((1 << sub) - 1)]
      } else {
        floor.subclass_books[cls][0]
      }
      if book_idx >= 0 && book_idx < codebooks.length() {
        y_list[offset] = codebook_decode(codebooks[book_idx], br)
      }
      offset = offset + 1
    }
  }
  // Synthesize floor curve
  let curve = synthesize_floor1_curve(floor, y_list, n, range)
  Some(curve)
}

///|
/// Synthesize floor type 1 curve from decoded Y values.
fn synthesize_floor1_curve(
  floor : VorbisFloorConfig,
  y_list : Array[Int],
  n : Int,
  range : Int,
) -> Array[Float] {
  let result : Array[Float] = Array::make(n, (0.0 : Float))
  let num_points = floor.x_list.length()
  if num_points < 2 {
    return result
  }
  // Use sorted indices for interpolation
  let sorted = floor.sorted_index
  // Amplitude rendering
  let mut lx = 0
  let mut ly = y_list[sorted[0]] * floor.multiplier
  for i in 1..<sorted.length() {
    let idx = sorted[i]
    let hx = floor.x_list[idx]
    let hy = y_list[idx] * floor.multiplier
    if lx < hx {
      render_line(result, lx, ly, hx, hy, n)
    }
    lx = hx
    ly = hy
    ignore(range)
  }
  // Fill remaining
  for i in lx..<n {
    result[i] = floor1_inverse_db(ly)
  }
  result
}

///|
/// Render a line segment into the floor curve.
fn render_line(
  output : Array[Float],
  x0 : Int,
  y0 : Int,
  x1 : Int,
  y1 : Int,
  n : Int,
) -> Unit {
  let dx = x1 - x0
  let dy = y1 - y0
  let adx = if dx >= 0 { dx } else { -dx }
  let ady = if dy >= 0 { dy } else { -dy }
  let base = dy / adx
  let sy = if dy < 0 { base - 1 } else { base + 1 }
  let mut x = x0
  let mut y = y0
  let mut err = 0
  let x_end = if x1 < n { x1 } else { n }
  while x < x_end {
    if x >= 0 && x < n {
      output[x] = floor1_inverse_db(y)
    }
    err = err + ady
    if err >= adx {
      err = err - adx
      y = y + sy
    } else {
      y = y + base
    }
    x = x + 1
  }
}

///|
/// Inverse dB lookup for floor1 values.
/// Simplified: maps integer floor values to linear amplitude.
fn floor1_inverse_db(val : Int) -> Float {
  if val <= 0 {
    return 0.0
  }
  // Simplified inverse dB table approximation
  // Full Vorbis spec has a 256-entry lookup table
  // We approximate: amplitude = 10^((val - 256) * 0.05 / 20)
  let db = Float::from_int(val - 256)
  let factor = db * (0.05 : Float) / (20.0 : Float)
  let ln10 : Float = 2.302585
  exp_approx(factor * ln10)
}

///|
/// Approximate exp(x) using Taylor series.
fn exp_approx(x : Float) -> Float {
  let mut result : Float = 1.0
  let mut term : Float = 1.0
  for i in 1..<=20 {
    term = term * x / Float::from_int(i)
    result = result + term
  }
  if result < 0.0 {
    0.0
  } else {
    result
  }
}
