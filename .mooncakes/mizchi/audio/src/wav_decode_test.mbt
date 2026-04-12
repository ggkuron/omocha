///|
/// Build a minimal WAV file in memory for testing.
/// 16-bit PCM, mono or stereo.
fn build_test_wav(
  channels : Int,
  sample_rate : Int,
  bits_per_sample : Int,
  samples : Array[Int],
) -> Bytes {
  let bytes_per_sample = bits_per_sample / 8
  let data_size = samples.length() * bytes_per_sample
  let fmt_chunk_size = 16
  let file_size = 4 + (8 + fmt_chunk_size) + (8 + data_size)
  let buf = Array::new(capacity=file_size + 8)
  // RIFF header
  buf.push(b'R')
  buf.push(b'I')
  buf.push(b'F')
  buf.push(b'F')
  push_u32le(buf, file_size)
  buf.push(b'W')
  buf.push(b'A')
  buf.push(b'V')
  buf.push(b'E')
  // fmt chunk
  buf.push(b'f')
  buf.push(b'm')
  buf.push(b't')
  buf.push(b' ')
  push_u32le(buf, fmt_chunk_size)
  push_u16le(buf, 1) // PCM
  push_u16le(buf, channels)
  push_u32le(buf, sample_rate)
  let block_align = channels * bytes_per_sample
  push_u32le(buf, sample_rate * block_align) // byte rate
  push_u16le(buf, block_align)
  push_u16le(buf, bits_per_sample)
  // data chunk
  buf.push(b'd')
  buf.push(b'a')
  buf.push(b't')
  buf.push(b'a')
  push_u32le(buf, data_size)
  for s in samples {
    if bits_per_sample == 16 {
      push_i16le(buf, s)
    } else {
      buf.push(s.to_byte())
    }
  }
  Bytes::from_array(buf[:])
}

///|
fn push_u16le(buf : Array[Byte], v : Int) -> Unit {
  buf.push((v & 0xFF).to_byte())
  buf.push(((v >> 8) & 0xFF).to_byte())
}

///|
fn push_u32le(buf : Array[Byte], v : Int) -> Unit {
  buf.push((v & 0xFF).to_byte())
  buf.push(((v >> 8) & 0xFF).to_byte())
  buf.push(((v >> 16) & 0xFF).to_byte())
  buf.push(((v >> 24) & 0xFF).to_byte())
}

///|
fn push_i16le(buf : Array[Byte], v : Int) -> Unit {
  let u = if v < 0 { v + 0x10000 } else { v }
  push_u16le(buf, u)
}

///|
test "decode_wav 16-bit mono" {
  // 2 samples: 0, 16384 (half max positive)
  let wav = build_test_wav(1, 44100, 16, [0, 16384])
  let buf = @audio.decode_wav(wav)
  inspect(buf.channels, content="1")
  inspect(buf.sample_rate, content="44100")
  inspect(@audio.frame_count(buf), content="2")
  assert_true(@audio.get_sample(buf, 0, 0) == (0.0 : Float))
  assert_true(
    Float::is_close(@audio.get_sample(buf, 1, 0), 0.5, absolute_tolerance=0.001),
  )
}

///|
test "decode_wav 16-bit negative" {
  // -32768 should map to -1.0
  let wav = build_test_wav(1, 44100, 16, [-32768])
  let buf = @audio.decode_wav(wav)
  assert_true(@audio.get_sample(buf, 0, 0) == (-1.0 : Float))
}

///|
test "decode_wav 16-bit max positive" {
  // 32767 should map to ~0.99997
  let wav = build_test_wav(1, 44100, 16, [32767])
  let buf = @audio.decode_wav(wav)
  assert_true(
    Float::is_close(@audio.get_sample(buf, 0, 0), 1.0, absolute_tolerance=0.001),
  )
}

///|
test "decode_wav 16-bit stereo" {
  // L=16384, R=-16384
  let wav = build_test_wav(2, 48000, 16, [16384, -16384])
  let buf = @audio.decode_wav(wav)
  inspect(buf.channels, content="2")
  inspect(@audio.frame_count(buf), content="1")
  assert_true(
    Float::is_close(@audio.get_sample(buf, 0, 0), 0.5, absolute_tolerance=0.001),
  )
  assert_true(
    Float::is_close(
      @audio.get_sample(buf, 0, 1),
      -0.5,
      absolute_tolerance=0.001,
    ),
  )
}

///|
test "decode_wav 8-bit mono" {
  // 128 -> 0.0, 0 -> -1.0, 255 -> ~0.992
  let wav = build_test_wav(1, 22050, 8, [128, 0, 255])
  let buf = @audio.decode_wav(wav)
  inspect(buf.channels, content="1")
  inspect(buf.sample_rate, content="22050")
  inspect(@audio.frame_count(buf), content="3")
  assert_true(
    Float::is_close(@audio.get_sample(buf, 0, 0), 0.0, absolute_tolerance=0.01),
  )
  assert_true(
    Float::is_close(@audio.get_sample(buf, 1, 0), -1.0, absolute_tolerance=0.01),
  )
  assert_true(
    Float::is_close(
      @audio.get_sample(buf, 2, 0),
      0.992,
      absolute_tolerance=0.01,
    ),
  )
}

///|
test "decode_wav invalid header" {
  let bytes = Bytes::make(44, b'\x00')
  let result : Result[@audio.AudioBuffer, _] = try? @audio.decode_wav(bytes)
  assert_true(result is Err(_))
}

///|
test "decode_wav too short" {
  let bytes = Bytes::make(10, b'\x00')
  let result : Result[@audio.AudioBuffer, _] = try? @audio.decode_wav(bytes)
  assert_true(result is Err(_))
}
