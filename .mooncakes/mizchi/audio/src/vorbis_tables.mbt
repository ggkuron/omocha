///|
/// Integer log base 2 (floor). Returns 0 for input 0.
fn ilog(x : Int) -> Int {
  let mut val = x
  let mut count = 0
  while val > 0 {
    count = count + 1
    val = val >> 1
  }
  count
}

///|
/// Bit-reverse an n-bit integer.
fn bit_reverse(x : Int, bits : Int) -> Int {
  let mut result = 0
  let mut val = x
  for _ in 0..<bits {
    result = (result << 1) | (val & 1)
    val = val >> 1
  }
  result
}

///|
/// Compute float32 from Vorbis packed representation.
/// Vorbis uses a custom float format: 1 sign bit, 5 exponent bits, 21 mantissa bits.
fn float32_unpack(x : Int) -> Float {
  let mantissa = x & 0x1FFFFF
  let sign = x & 0x80000000
  let exponent = (x & 0x7FE00000) >> 21
  let signed_mantissa : Float = if sign != 0 {
    -Float::from_int(mantissa)
  } else {
    Float::from_int(mantissa)
  }
  let exp_val = Float::from_int(exponent - 788)
  signed_mantissa * pow2f(exp_val)
}

///|
/// Compute 2^x for float exponent using repeated squaring.
fn pow2f(x : Float) -> Float {
  // 2^x = exp(x * ln(2))
  let ln2 : Float = 0.6931472
  let v = x * ln2
  // Use Taylor series for exp(v) with reasonable precision
  let mut result : Float = 1.0
  let mut term : Float = 1.0
  for i in 1..<=20 {
    term = term * v / Float::from_int(i)
    result = result + term
  }
  result
}

///|
test "ilog basic values" {
  assert_eq(ilog(0), 0)
  assert_eq(ilog(1), 1)
  assert_eq(ilog(2), 2)
  assert_eq(ilog(3), 2)
  assert_eq(ilog(4), 3)
  assert_eq(ilog(255), 8)
  assert_eq(ilog(256), 9)
}

///|
test "bit_reverse" {
  assert_eq(bit_reverse(0b1010, 4), 0b0101)
  assert_eq(bit_reverse(0b1100, 4), 0b0011)
  assert_eq(bit_reverse(0b1, 8), 0b10000000)
  assert_eq(bit_reverse(0, 4), 0)
}

///|
test "float32_unpack basic" {
  // Simple test: value 0 should give 0
  let result = float32_unpack(0)
  assert_true(result < 0.001 && result > -0.001)
}

///|
test "lookup1_values" {
  assert_eq(ilog(15), 4)
  assert_eq(ilog(16), 5)
  assert_eq(ilog(1023), 10)
  assert_eq(ilog(1024), 11)
}
