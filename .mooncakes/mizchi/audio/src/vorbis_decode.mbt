///|
/// Vorbis stream configuration parsed from headers.
struct VorbisInfo {
  channels : Int
  sample_rate : Int
  blocksize_0 : Int
  blocksize_1 : Int
  codebooks : Array[VorbisCodebook]
  floors : Array[VorbisFloorConfig]
  residues : Array[VorbisResidueConfig]
  mappings : Array[VorbisMapping]
  modes : Array[VorbisMode]
} derive(Show)

///|
/// Vorbis floor type 1 configuration.
struct VorbisFloorConfig {
  partitions : Int
  partition_class : Array[Int]
  class_dimensions : Array[Int]
  class_subclasses : Array[Int]
  class_masterbooks : Array[Int]
  subclass_books : Array[Array[Int]]
  multiplier : Int
  x_list : Array[Int]
  sorted_index : Array[Int]
} derive(Show)

///|
/// Vorbis residue configuration.
struct VorbisResidueConfig {
  residue_type : Int
  begin : Int
  end_ : Int
  partition_size : Int
  classifications : Int
  classbook : Int
  cascade : Array[Int]
  books : Array[Array[Int]]
} derive(Show)

///|
/// Vorbis channel mapping.
struct VorbisMapping {
  submaps : Int
  coupling_steps : Int
  coupling_magnitude : Array[Int]
  coupling_angle : Array[Int]
  mux : Array[Int]
  submap_floor : Array[Int]
  submap_residue : Array[Int]
} derive(Show)

///|
/// Vorbis mode configuration.
struct VorbisMode {
  block_flag : Bool
  mapping : Int
} derive(Show)

///|
/// Parse the Vorbis identification header.
fn parse_identification_header(
  data : Bytes,
) -> (Int, Int, Int, Int) raise AudioError {
  if data.length() < 30 {
    raise AudioError::DecodeFailed("Vorbis: identification header too short")
  }
  // Check packet type (1) and "vorbis" magic
  if data[0].to_int() != 1 {
    raise AudioError::DecodeFailed("Vorbis: not an identification header")
  }
  if data[1].to_int() != 0x76 ||
    data[2].to_int() != 0x6F ||
    data[3].to_int() != 0x72 ||
    data[4].to_int() != 0x62 ||
    data[5].to_int() != 0x69 ||
    data[6].to_int() != 0x73 {
    raise AudioError::DecodeFailed("Vorbis: invalid magic")
  }
  let _version = read_u32le(data, 7)
  let channels = read_u8(data, 11)
  let sample_rate = read_u32le(data, 12)
  // Skip bitrate fields (12 bytes)
  let blocksize_byte = read_u8(data, 28)
  let blocksize_0 = 1 << (blocksize_byte & 0x0F)
  let blocksize_1 = 1 << ((blocksize_byte >> 4) & 0x0F)
  (channels, sample_rate, blocksize_0, blocksize_1)
}

///|
/// Parse the Vorbis setup header (codebooks, floors, residues, mappings, modes).
fn parse_setup_header(
  data : Bytes,
  channels : Int,
) -> (
  Array[VorbisCodebook],
  Array[VorbisFloorConfig],
  Array[VorbisResidueConfig],
  Array[VorbisMapping],
  Array[VorbisMode],
) raise AudioError {
  if data.length() < 7 {
    raise AudioError::DecodeFailed("Vorbis: setup header too short")
  }
  if data[0].to_int() != 5 {
    raise AudioError::DecodeFailed("Vorbis: not a setup header")
  }
  // Check "vorbis" magic
  if data[1].to_int() != 0x76 ||
    data[2].to_int() != 0x6F ||
    data[3].to_int() != 0x72 ||
    data[4].to_int() != 0x62 ||
    data[5].to_int() != 0x69 ||
    data[6].to_int() != 0x73 {
    raise AudioError::DecodeFailed("Vorbis: invalid setup magic")
  }
  let br = new_bit_reader(data)
  // Skip packet type + "vorbis" = 7 bytes
  br.bit_pos = 7 * 8
  // Codebooks
  let codebook_count = read_bits(br, 8) + 1
  let codebooks : Array[VorbisCodebook] = []
  for _ in 0..<codebook_count {
    codebooks.push(parse_codebook(br))
  }
  // Time domain transforms (skip)
  let time_count = read_bits(br, 6) + 1
  for _ in 0..<time_count {
    read_bits(br, 16) |> ignore
  }
  // Floors
  let floor_count = read_bits(br, 6) + 1
  let floors : Array[VorbisFloorConfig] = []
  for _ in 0..<floor_count {
    floors.push(parse_floor_config(br))
  }
  // Residues
  let residue_count = read_bits(br, 6) + 1
  let residues : Array[VorbisResidueConfig] = []
  for _ in 0..<residue_count {
    residues.push(parse_residue_config(br, codebooks))
  }
  // Mappings
  let mapping_count = read_bits(br, 6) + 1
  let mappings : Array[VorbisMapping] = []
  for _ in 0..<mapping_count {
    mappings.push(parse_mapping(br, channels))
  }
  // Modes
  let mode_count = read_bits(br, 6) + 1
  let modes : Array[VorbisMode] = []
  for _ in 0..<mode_count {
    let block_flag = read_bit(br)
    let _window_type = read_bits(br, 16)
    let _transform_type = read_bits(br, 16)
    let mapping = read_bits(br, 8)
    modes.push(VorbisMode::{ block_flag, mapping })
  }
  (codebooks, floors, residues, mappings, modes)
}

///|
/// Parse a floor type 1 configuration from the bitstream.
fn parse_floor_config(br : BitReader) -> VorbisFloorConfig raise AudioError {
  let floor_type = read_bits(br, 16)
  if floor_type != 1 {
    raise AudioError::DecodeFailed(
      "Vorbis: only floor type 1 is supported (got \{floor_type})",
    )
  }
  let partitions = read_bits(br, 5)
  let partition_class : Array[Int] = []
  let mut max_class = -1
  for _ in 0..<partitions {
    let cls = read_bits(br, 4)
    partition_class.push(cls)
    if cls > max_class {
      max_class = cls
    }
  }
  let class_dimensions : Array[Int] = []
  let class_subclasses : Array[Int] = []
  let class_masterbooks : Array[Int] = []
  let subclass_books : Array[Array[Int]] = []
  for _ in 0..<=max_class {
    let dim = read_bits(br, 3) + 1
    class_dimensions.push(dim)
    let sub = read_bits(br, 2)
    class_subclasses.push(sub)
    let master = if sub > 0 { read_bits(br, 8) } else { 0 }
    class_masterbooks.push(master)
    let books : Array[Int] = []
    let num_sub = 1 << sub
    for _ in 0..<num_sub {
      books.push(read_bits(br, 8) - 1)
    }
    subclass_books.push(books)
  }
  let multiplier = read_bits(br, 2) + 1
  let range_bits = read_bits(br, 4)
  let x_list : Array[Int] = [0, 1 << range_bits]
  for i in 0..<partitions {
    let cls = partition_class[i]
    let dim = class_dimensions[cls]
    for _ in 0..<dim {
      x_list.push(read_bits(br, range_bits))
    }
  }
  // Compute sorted indices
  let sorted_index : Array[Int] = []
  for i in 0..<x_list.length() {
    sorted_index.push(i)
  }
  sorted_index.sort_by(fn(a, b) { x_list[a].compare(x_list[b]) })
  VorbisFloorConfig::{
    partitions,
    partition_class,
    class_dimensions,
    class_subclasses,
    class_masterbooks,
    subclass_books,
    multiplier,
    x_list,
    sorted_index,
  }
}

///|
/// Parse a residue configuration.
fn parse_residue_config(
  br : BitReader,
  codebooks : Array[VorbisCodebook],
) -> VorbisResidueConfig raise AudioError {
  let residue_type = read_bits(br, 16)
  if residue_type > 2 {
    raise AudioError::DecodeFailed(
      "Vorbis: unsupported residue type \{residue_type}",
    )
  }
  let begin = read_bits(br, 24)
  let end_ = read_bits(br, 24)
  let partition_size = read_bits(br, 24) + 1
  let classifications = read_bits(br, 6) + 1
  let classbook = read_bits(br, 8)
  let cascade : Array[Int] = []
  for _ in 0..<classifications {
    let mut high_bits = 0
    let low_bits = read_bits(br, 3)
    let bitflag = read_bit(br)
    if bitflag {
      high_bits = read_bits(br, 5)
    }
    cascade.push(high_bits * 8 + low_bits)
  }
  let books : Array[Array[Int]] = []
  for i in 0..<classifications {
    let entry : Array[Int] = []
    for j in 0..<8 {
      if (cascade[i] & (1 << j)) != 0 {
        entry.push(read_bits(br, 8))
      } else {
        entry.push(-1)
      }
    }
    books.push(entry)
  }
  ignore(codebooks)
  VorbisResidueConfig::{
    residue_type,
    begin,
    end_,
    partition_size,
    classifications,
    classbook,
    cascade,
    books,
  }
}

///|
/// Parse a mapping configuration.
fn parse_mapping(
  br : BitReader,
  channels : Int,
) -> VorbisMapping raise AudioError {
  let mapping_type = read_bits(br, 16)
  if mapping_type != 0 {
    raise AudioError::DecodeFailed("Vorbis: unsupported mapping type")
  }
  let submaps = if read_bit(br) { read_bits(br, 4) + 1 } else { 1 }
  let mut coupling_steps = 0
  let coupling_magnitude : Array[Int] = []
  let coupling_angle : Array[Int] = []
  if read_bit(br) {
    coupling_steps = read_bits(br, 8) + 1
    let ch_bits = ilog(channels - 1)
    for _ in 0..<coupling_steps {
      coupling_magnitude.push(read_bits(br, ch_bits))
      coupling_angle.push(read_bits(br, ch_bits))
    }
  }
  let reserved = read_bits(br, 2)
  if reserved != 0 {
    raise AudioError::DecodeFailed("Vorbis: mapping reserved bits non-zero")
  }
  let mux : Array[Int] = []
  if submaps > 1 {
    for _ in 0..<channels {
      mux.push(read_bits(br, 4))
    }
  } else {
    for _ in 0..<channels {
      mux.push(0)
    }
  }
  let submap_floor : Array[Int] = []
  let submap_residue : Array[Int] = []
  for _ in 0..<submaps {
    read_bits(br, 8) |> ignore // unused time config
    submap_floor.push(read_bits(br, 8))
    submap_residue.push(read_bits(br, 8))
  }
  VorbisMapping::{
    submaps,
    coupling_steps,
    coupling_magnitude,
    coupling_angle,
    mux,
    submap_floor,
    submap_residue,
  }
}

///|
/// Parse all three Vorbis headers from an OGG demuxer and return VorbisInfo.
fn parse_vorbis_headers(demuxer : OggDemuxer) -> VorbisInfo raise AudioError {
  // Packet 1: Identification header
  let id_packet = match next_packet(demuxer) {
    Some(p) => p
    None =>
      raise AudioError::DecodeFailed("Vorbis: missing identification header")
  }
  let (channels, sample_rate, blocksize_0, blocksize_1) = parse_identification_header(
    id_packet,
  )
  // Packet 2: Comment header (skip)
  match next_packet(demuxer) {
    Some(_) => ()
    None => raise AudioError::DecodeFailed("Vorbis: missing comment header")
  }
  // Packet 3: Setup header
  let setup_packet = match next_packet(demuxer) {
    Some(p) => p
    None => raise AudioError::DecodeFailed("Vorbis: missing setup header")
  }
  let (codebooks, floors, residues, mappings, modes) = parse_setup_header(
    setup_packet, channels,
  )
  VorbisInfo::{
    channels,
    sample_rate,
    blocksize_0,
    blocksize_1,
    codebooks,
    floors,
    residues,
    mappings,
    modes,
  }
}

///|
/// Top-level OGG/Vorbis decode function.
/// Decodes an OGG/Vorbis file from raw bytes into an AudioBuffer.
pub fn decode_ogg(raw : Bytes) -> AudioBuffer raise AudioError {
  let demuxer = new_ogg_demuxer(raw)
  let info = parse_vorbis_headers(demuxer)
  // Decode audio packets
  let samples : Array[Float] = []
  let mut prev_left : Array[Float] = []
  let mut prev_right : Array[Float] = []
  while true {
    let packet = match next_packet(demuxer) {
      Some(p) => p
      None => break
    }
    let (left, right) = decode_audio_packet(info, packet)
    // Overlap-add with previous window
    if prev_left.length() > 0 {
      let overlap = if prev_left.length() < left.length() {
        prev_left.length()
      } else {
        left.length()
      }
      let half_overlap = overlap / 2
      // Add overlapping region
      for i in 0..<half_overlap {
        if info.channels >= 2 {
          samples.push(
            prev_left[prev_left.length() - half_overlap + i] + left[i],
          )
          samples.push(
            prev_right[prev_right.length() - half_overlap + i] + right[i],
          )
        } else {
          samples.push(
            prev_left[prev_left.length() - half_overlap + i] + left[i],
          )
        }
      }
      // Add non-overlapping part of current window
      for i in half_overlap..<left.length() {
        if info.channels >= 2 {
          samples.push(left[i])
          samples.push(right[i])
        } else {
          samples.push(left[i])
        }
      }
    }
    prev_left = left
    prev_right = right
  }
  let data = FixedArray::make(samples.length(), (0.0 : Float))
  for i in 0..<samples.length() {
    data[i] = samples[i]
  }
  AudioBuffer::{ channels: info.channels, sample_rate: info.sample_rate, data }
}
