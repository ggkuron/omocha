///|
/// Create a new OGG demuxer from raw bytes.
fn new_ogg_demuxer(data : Bytes) -> OggDemuxer {
  OggDemuxer::{ data, offset: 0, current_packet: [] }
}

///|
/// Read the next OGG page from the demuxer.
fn read_ogg_page(demuxer : OggDemuxer) -> OggPage raise AudioError {
  let data = demuxer.data
  let off = demuxer.offset
  // Check sync pattern "OggS"
  if off + 27 > data.length() {
    raise AudioError::DecodeFailed("OGG: unexpected end of data")
  }
  if data[off].to_int() != 0x4F ||
    data[off + 1].to_int() != 0x67 ||
    data[off + 2].to_int() != 0x67 ||
    data[off + 3].to_int() != 0x53 {
    raise AudioError::DecodeFailed("OGG: invalid sync pattern")
  }
  let _version = read_u8(data, off + 4)
  let header_type = read_u8(data, off + 5)
  let granule_position = read_u64le(data, off + 6)
  let serial_number = read_u32le(data, off + 14)
  let page_sequence = read_u32le(data, off + 18)
  let num_segments = read_u8(data, off + 26)
  if off + 27 + num_segments > data.length() {
    raise AudioError::DecodeFailed("OGG: truncated segment table")
  }
  // Read segment table
  let segment_sizes : Array[Int] = []
  let mut data_size = 0
  for i in 0..<num_segments {
    let seg_size = read_u8(data, off + 27 + i)
    segment_sizes.push(seg_size)
    data_size = data_size + seg_size
  }
  let header_size = 27 + num_segments
  if off + header_size + data_size > data.length() {
    raise AudioError::DecodeFailed("OGG: truncated page data")
  }
  // Extract segments as individual byte arrays
  let segments : Array[Bytes] = []
  let mut seg_offset = off + header_size
  for i in 0..<num_segments {
    let seg_size = segment_sizes[i]
    if seg_size > 0 {
      let arr : Array[Byte] = []
      for j in 0..<seg_size {
        arr.push(data[seg_offset + j])
      }
      segments.push(Bytes::from_array(arr))
    } else {
      segments.push(Bytes::from_array([]))
    }
    seg_offset = seg_offset + seg_size
  }
  demuxer.offset = off + header_size + data_size
  OggPage::{
    header_type,
    granule_position,
    serial_number,
    page_sequence,
    segments,
  }
}

///|
/// Concatenate multiple byte arrays into one.
fn concat_bytes(parts : Array[Bytes]) -> Bytes {
  let arr : Array[Byte] = []
  for part in parts {
    for i in 0..<part.length() {
      arr.push(part[i])
    }
  }
  Bytes::from_array(arr)
}

///|
/// Read the next complete Vorbis packet from the OGG stream.
/// Packets can span multiple segments (255-byte segments continue, <255 terminates).
fn next_packet(demuxer : OggDemuxer) -> Bytes? raise AudioError {
  let mut packet_parts : Array[Bytes] = demuxer.current_packet
  demuxer.current_packet = []
  while true {
    if demuxer.offset >= demuxer.data.length() {
      if packet_parts.length() > 0 {
        let result = concat_bytes(packet_parts)
        return Some(result)
      }
      return None
    }
    let page = read_ogg_page(demuxer)
    for i, seg in page.segments {
      packet_parts.push(seg)
      let seg_size = seg.length()
      if seg_size < 255 {
        let packet = concat_bytes(packet_parts)
        packet_parts = []
        if i + 1 < page.segments.length() {
          let remaining : Array[Bytes] = []
          for j in (i + 1)..<page.segments.length() {
            remaining.push(page.segments[j])
          }
          demuxer.current_packet = remaining
          return Some(packet)
        }
        return Some(packet)
      }
    }
  }
  None
}

///|
/// Build a minimal OGG page with given segments (test helper).
fn build_ogg_page(
  header_type : Int,
  serial : Int,
  page_seq : Int,
  segment_data : Array[Array[Byte]],
) -> Array[Byte] {
  let result : Array[Byte] = []
  // Sync pattern "OggS"
  result.push(b'\x4F')
  result.push(b'\x67')
  result.push(b'\x67')
  result.push(b'\x53')
  // Version
  result.push(b'\x00')
  // Header type
  result.push(header_type.to_byte())
  // Granule position (8 bytes, all zeros)
  for _ in 0..<8 {
    result.push(b'\x00')
  }
  // Serial number (4 bytes LE)
  result.push((serial & 0xFF).to_byte())
  result.push(((serial >> 8) & 0xFF).to_byte())
  result.push(((serial >> 16) & 0xFF).to_byte())
  result.push(((serial >> 24) & 0xFF).to_byte())
  // Page sequence (4 bytes LE)
  result.push((page_seq & 0xFF).to_byte())
  result.push(((page_seq >> 8) & 0xFF).to_byte())
  result.push(((page_seq >> 16) & 0xFF).to_byte())
  result.push(((page_seq >> 24) & 0xFF).to_byte())
  // CRC (4 bytes, zeros - we skip CRC validation)
  for _ in 0..<4 {
    result.push(b'\x00')
  }
  // Number of segments
  let num_segments = segment_data.length()
  result.push(num_segments.to_byte())
  // Segment table
  for seg in segment_data {
    result.push(seg.length().to_byte())
  }
  // Segment data
  for seg in segment_data {
    for b in seg {
      result.push(b)
    }
  }
  result
}

///|
test "ogg demuxer single page single packet" {
  let seg_data : Array[Byte] = [b'\x01', b'\x02', b'\x03']
  let page = build_ogg_page(0x02, 1, 0, [seg_data]) // BOS flag
  let bytes = Bytes::from_array(page)
  let demuxer = new_ogg_demuxer(bytes)
  let packet = next_packet(demuxer)
  match packet {
    Some(p) => {
      assert_eq(p.length(), 3)
      assert_eq(p[0].to_int(), 1)
      assert_eq(p[1].to_int(), 2)
      assert_eq(p[2].to_int(), 3)
    }
    None => assert_true(false)
  }
}

///|
test "ogg demuxer multiple packets in one page" {
  let seg1 : Array[Byte] = [b'\xAA', b'\xBB']
  let seg2 : Array[Byte] = [b'\xCC']
  let page = build_ogg_page(0x02, 1, 0, [seg1, seg2])
  let bytes = Bytes::from_array(page)
  let demuxer = new_ogg_demuxer(bytes)
  let p1 = next_packet(demuxer)
  match p1 {
    Some(p) => assert_eq(p.length(), 2)
    None => assert_true(false)
  }
  let p2 = next_packet(demuxer)
  match p2 {
    Some(p) => {
      assert_eq(p.length(), 1)
      assert_eq(p[0].to_int(), 0xCC)
    }
    None => assert_true(false)
  }
}

///|
test "ogg demuxer returns None when exhausted" {
  let seg : Array[Byte] = [b'\x01']
  let page = build_ogg_page(0x02, 1, 0, [seg])
  let bytes = Bytes::from_array(page)
  let demuxer = new_ogg_demuxer(bytes)
  next_packet(demuxer) |> ignore
  let p = next_packet(demuxer)
  assert_true(p is None)
}

///|
test "ogg demuxer error on invalid sync" {
  let bytes = Bytes::from_array([b'\x00', b'\x00', b'\x00', b'\x00'])
  let demuxer = new_ogg_demuxer(bytes)
  let mut caught = false
  try next_packet(demuxer) |> ignore catch {
    AudioError::DecodeFailed(_) => caught = true
    _ => ()
  }
  assert_true(caught)
}
