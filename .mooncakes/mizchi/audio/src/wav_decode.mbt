///|
/// Decode a WAV file from raw bytes into an AudioBuffer.
/// Supports PCM format (format=1), 8-bit and 16-bit.
pub fn decode_wav(raw : Bytes) -> AudioBuffer raise AudioError {
  // Minimum WAV header size
  if raw.length() < 44 {
    raise AudioError::InvalidFormat("WAV too short")
  }
  // RIFF header
  if raw[0] != b'R' || raw[1] != b'I' || raw[2] != b'F' || raw[3] != b'F' {
    raise AudioError::InvalidFormat("Missing RIFF header")
  }
  // WAVE format
  if raw[8] != b'W' || raw[9] != b'A' || raw[10] != b'V' || raw[11] != b'E' {
    raise AudioError::InvalidFormat("Missing WAVE format")
  }
  // Find fmt chunk
  let (fmt_offset, _fmt_size) = find_chunk(raw, b'f', b'm', b't', b' ') catch {
    _ => raise AudioError::InvalidFormat("Missing fmt chunk")
  }
  let audio_format = read_u16le(raw, fmt_offset)
  if audio_format != 1 {
    raise AudioError::InvalidFormat("Not PCM format")
  }
  let channels = read_u16le(raw, fmt_offset + 2)
  let sample_rate = read_u32le(raw, fmt_offset + 4)
  let bits_per_sample = read_u16le(raw, fmt_offset + 14)
  if bits_per_sample != 8 && bits_per_sample != 16 {
    raise AudioError::DecodeFailed("Unsupported bits per sample")
  }
  // Find data chunk
  let (data_offset, data_size) = find_chunk(raw, b'd', b'a', b't', b'a') catch {
    _ => raise AudioError::InvalidFormat("Missing data chunk")
  }
  let bytes_per_sample = bits_per_sample / 8
  let total_samples = data_size / bytes_per_sample
  let frames = total_samples / channels
  let buf = new_audio_buffer(channels, sample_rate, frames)
  for i in 0..<total_samples {
    let byte_offset = data_offset + i * bytes_per_sample
    let sample : Float = if bits_per_sample == 16 {
      // 16-bit signed PCM: normalize to [-1.0, 1.0]
      let raw_val = read_i16le(raw, byte_offset)
      Float::from_int(raw_val) / 32768.0
    } else {
      // 8-bit unsigned PCM: 0..255 -> [-1.0, 1.0]
      let raw_val = raw[byte_offset].to_int()
      (Float::from_int(raw_val) - 128.0) / 128.0
    }
    let frame = i / channels
    let channel = i % channels
    set_sample(buf, frame, channel, sample)
  }
  buf
}

///|
/// Find a RIFF chunk by its 4-byte ID. Returns (data_offset, data_size).
fn find_chunk(
  raw : Bytes,
  c0 : Byte,
  c1 : Byte,
  c2 : Byte,
  c3 : Byte,
) -> (Int, Int) raise AudioError {
  let len = raw.length()
  // Start after RIFF header (12 bytes)
  let mut offset = 12
  while offset + 8 <= len {
    if raw[offset] == c0 &&
      raw[offset + 1] == c1 &&
      raw[offset + 2] == c2 &&
      raw[offset + 3] == c3 {
      let chunk_size = read_u32le(raw, offset + 4)
      return (offset + 8, chunk_size)
    }
    let chunk_size = read_u32le(raw, offset + 4)
    offset = offset + 8 + chunk_size
    // Chunks are word-aligned
    if offset % 2 != 0 {
      offset = offset + 1
    }
  }
  raise AudioError::InvalidFormat("Chunk not found")
}
