///|
/// Perform inverse MDCT (IMDCT) on the input spectrum.
/// Takes N/2 spectral coefficients and produces N time-domain samples.
fn imdct(input : Array[Float], n : Int) -> Array[Float] {
  let half_n = n / 2
  let output : Array[Float] = Array::make(n, (0.0 : Float))
  let pi = Float::from_double(@math.PI)
  // Direct IMDCT computation (O(N^2) - correct but slow)
  // For production use, this should be replaced with FFT-based O(N log N)
  for i in 0..<n {
    let mut sum : Float = 0.0
    for k in 0..<half_n {
      let fi = Float::from_int(i)
      let fk = Float::from_int(k)
      let fn_ = Float::from_int(n)
      let angle = pi /
        fn_ *
        (fi + 0.5 + fn_ / (4.0 : Float)) *
        ((2.0 : Float) * fk + (1.0 : Float))
      sum = sum + input[k] * @math.cosf(angle)
    }
    output[i] = sum * (2.0 : Float) / Float::from_int(n)
  }
  output
}

///|
test "imdct basic" {
  // IMDCT of all zeros should give all zeros
  let input = Array::make(4, (0.0 : Float))
  let output = imdct(input, 8)
  assert_eq(output.length(), 8)
  for i in 0..<8 {
    let v = output[i]
    let abs_v = if v >= 0.0 { v } else { -v }
    assert_true(abs_v < 0.001)
  }
}

///|
test "imdct impulse response" {
  // Single impulse at DC should produce constant output
  let input : Array[Float] = [1.0, 0.0, 0.0, 0.0]
  let output = imdct(input, 8)
  assert_eq(output.length(), 8)
  // Output should not be all zeros
  let mut max_val : Float = 0.0
  for i in 0..<8 {
    let v = output[i]
    let abs_v = if v >= 0.0 { v } else { -v }
    if abs_v > max_val {
      max_val = abs_v
    }
  }
  assert_true(max_val > 0.01)
}
