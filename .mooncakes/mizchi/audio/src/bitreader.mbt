///|
/// Create a new LSB-first bit reader from bytes.
fn new_bit_reader(data : Bytes) -> BitReader {
  BitReader::{ data, length_bits: data.length() * 8, bit_pos: 0 }
}

///|
/// Read n bits from the bitstream (LSB-first). Returns the value.
fn read_bits(br : BitReader, n : Int) -> Int raise AudioError {
  if n == 0 {
    return 0
  }
  if br.bit_pos + n > br.length_bits {
    raise AudioError::DecodeFailed("read_bits: not enough bits remaining")
  }
  let mut result = 0
  for i in 0..<n {
    let byte_idx = (br.bit_pos + i) / 8
    let bit_idx = (br.bit_pos + i) % 8
    let bit = (br.data[byte_idx].to_int() >> bit_idx) & 1
    result = result | (bit << i)
  }
  br.bit_pos = br.bit_pos + n
  result
}

///|
/// Read a single bit from the bitstream.
fn read_bit(br : BitReader) -> Bool raise AudioError {
  if br.bit_pos >= br.length_bits {
    raise AudioError::DecodeFailed("read_bit: no bits remaining")
  }
  let byte_idx = br.bit_pos / 8
  let bit_idx = br.bit_pos % 8
  let bit = (br.data[byte_idx].to_int() >> bit_idx) & 1
  br.bit_pos = br.bit_pos + 1
  bit == 1
}

///|
/// Return the number of bits remaining in the reader.
fn bits_remaining(br : BitReader) -> Int {
  br.length_bits - br.bit_pos
}

///|
/// Align the bit position to the next byte boundary.
fn byte_align(br : BitReader) -> Unit {
  let remainder = br.bit_pos % 8
  if remainder != 0 {
    br.bit_pos = br.bit_pos + (8 - remainder)
  }
}

///|
test "bitreader LSB-first read_bits" {
  // Byte 0xB1 = 10110001
  // LSB-first: bits 0,1,2 = 1,0,0 = 0b001 = 1
  let data = Bytes::from_array([b'\xB1'])
  let br = new_bit_reader(data)
  let v = read_bits(br, 3)
  assert_eq(v, 1)
}

///|
test "bitreader read_bits multi-byte" {
  let data = Bytes::from_array([b'\xFF', b'\x00'])
  let br = new_bit_reader(data)
  let v = read_bits(br, 8)
  assert_eq(v, 255)
  let v2 = read_bits(br, 8)
  assert_eq(v2, 0)
}

///|
test "bitreader read_bits across byte boundary" {
  // 0xAB = 10101011, 0xCD = 11001101
  // Read 12 bits LSB-first: bits 0-7 from byte0 = 0xAB = 171
  // bits 8-11 from byte1 bits 0-3 = low nibble of 0xCD = 0xD = 13
  // result = 171 | (13 << 8) = 0xDAB
  let data = Bytes::from_array([b'\xAB', b'\xCD'])
  let br = new_bit_reader(data)
  let v = read_bits(br, 12)
  assert_eq(v, 0xDAB)
}

///|
test "bitreader read_bit" {
  let data = Bytes::from_array([b'\x05']) // 00000101
  let br = new_bit_reader(data)
  assert_true(read_bit(br) == true) // bit 0 = 1
  assert_true(read_bit(br) == false) // bit 1 = 0
  assert_true(read_bit(br) == true) // bit 2 = 1
  assert_true(read_bit(br) == false) // bit 3 = 0
}

///|
test "bitreader bits_remaining" {
  let data = Bytes::from_array([b'\x00', b'\x00'])
  let br = new_bit_reader(data)
  assert_eq(bits_remaining(br), 16)
  read_bits(br, 5) |> ignore
  assert_eq(bits_remaining(br), 11)
}

///|
test "bitreader byte_align" {
  let data = Bytes::from_array([b'\xFF', b'\xAB'])
  let br = new_bit_reader(data)
  read_bits(br, 3) |> ignore
  byte_align(br)
  assert_eq(bits_remaining(br), 8)
  let v = read_bits(br, 8)
  assert_eq(v, 0xAB)
}

///|
test "bitreader read_bits error on overflow" {
  let data = Bytes::from_array([b'\xFF'])
  let br = new_bit_reader(data)
  let mut caught = false
  try read_bits(br, 9) |> ignore catch {
    AudioError::DecodeFailed(_) => caught = true
    _ => ()
  }
  assert_true(caught)
}

///|
test "bitreader read zero bits" {
  let data = Bytes::from_array([b'\xFF'])
  let br = new_bit_reader(data)
  let v = read_bits(br, 0)
  assert_eq(v, 0)
  assert_eq(bits_remaining(br), 8)
}
