///|
/// Read a single unsigned byte from bytes at offset.
fn read_u8(bytes : Bytes, offset : Int) -> Int {
  bytes[offset].to_int()
}

///|
/// Read a little-endian unsigned 64-bit integer from bytes at offset.
fn read_u64le(bytes : Bytes, offset : Int) -> Int64 {
  let b0 = bytes[offset].to_int().to_int64()
  let b1 = bytes[offset + 1].to_int().to_int64()
  let b2 = bytes[offset + 2].to_int().to_int64()
  let b3 = bytes[offset + 3].to_int().to_int64()
  let b4 = bytes[offset + 4].to_int().to_int64()
  let b5 = bytes[offset + 5].to_int().to_int64()
  let b6 = bytes[offset + 6].to_int().to_int64()
  let b7 = bytes[offset + 7].to_int().to_int64()
  b0 |
  (b1 << 8) |
  (b2 << 16) |
  (b3 << 24) |
  (b4 << 32) |
  (b5 << 40) |
  (b6 << 48) |
  (b7 << 56)
}

///|
/// Read a little-endian unsigned 16-bit integer from bytes at offset.
fn read_u16le(bytes : Bytes, offset : Int) -> Int {
  bytes[offset].to_int() | (bytes[offset + 1].to_int() << 8)
}

///|
/// Read a little-endian unsigned 32-bit integer from bytes at offset.
fn read_u32le(bytes : Bytes, offset : Int) -> Int {
  bytes[offset].to_int() |
  (bytes[offset + 1].to_int() << 8) |
  (bytes[offset + 2].to_int() << 16) |
  (bytes[offset + 3].to_int() << 24)
}

///|
/// Read a little-endian signed 16-bit integer from bytes at offset.
fn read_i16le(bytes : Bytes, offset : Int) -> Int {
  let v = read_u16le(bytes, offset)
  if v >= 0x8000 {
    v - 0x10000
  } else {
    v
  }
}

///|
test "read_u16le" {
  // 0x0201 = 513
  let bytes = Bytes::from_array([b'\x01', b'\x02'])
  inspect(read_u16le(bytes, 0), content="513")
}

///|
test "read_u16le max" {
  // 0xFFFF = 65535
  let bytes = Bytes::from_array([b'\xFF', b'\xFF'])
  inspect(read_u16le(bytes, 0), content="65535")
}

///|
test "read_u32le" {
  // 0x04030201 = 67305985
  let bytes = Bytes::from_array([b'\x01', b'\x02', b'\x03', b'\x04'])
  inspect(read_u32le(bytes, 0), content="67305985")
}

///|
test "read_u32le with offset" {
  let bytes = Bytes::from_array([
    b'\x00', b'\x00', b'\x01', b'\x02', b'\x03', b'\x04',
  ])
  inspect(read_u32le(bytes, 2), content="67305985")
}

///|
test "read_i16le positive" {
  // 0x7FFF = 32767
  let bytes = Bytes::from_array([b'\xFF', b'\x7F'])
  inspect(read_i16le(bytes, 0), content="32767")
}

///|
test "read_i16le negative" {
  // 0x8000 = -32768
  let bytes = Bytes::from_array([b'\x00', b'\x80'])
  inspect(read_i16le(bytes, 0), content="-32768")
}

///|
test "read_i16le minus one" {
  // 0xFFFF = -1
  let bytes = Bytes::from_array([b'\xFF', b'\xFF'])
  inspect(read_i16le(bytes, 0), content="-1")
}

///|
test "read_i16le zero" {
  let bytes = Bytes::from_array([b'\x00', b'\x00'])
  inspect(read_i16le(bytes, 0), content="0")
}
