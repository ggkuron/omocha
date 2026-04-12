///|
/// Compute Vorbis window function value.
/// w(i) = sin(π/2 · sin²(π(i+0.5)/n))
fn vorbis_window(i : Int, n : Int) -> Float {
  let pi = Float::from_double(@math.PI)
  let half_pi = pi / (2.0 : Float)
  let inner = pi * (Float::from_int(i) + 0.5) / Float::from_int(n)
  let sin_inner = @math.sinf(inner)
  let sin_sq = sin_inner * sin_inner
  @math.sinf(half_pi * sin_sq)
}

///|
/// Apply the Vorbis window function to a buffer in place.
fn apply_vorbis_window(data : Array[Float], n : Int) -> Unit {
  for i in 0..<n {
    if i < data.length() {
      data[i] = data[i] * vorbis_window(i, n)
    }
  }
}

///|
/// Decode a single Vorbis audio packet into per-channel sample arrays.
fn decode_audio_packet(
  info : VorbisInfo,
  packet : Bytes,
) -> (Array[Float], Array[Float]) raise AudioError {
  let br = new_bit_reader(packet)
  // Packet type must be 0 (audio)
  let packet_type = read_bit(br)
  if packet_type {
    raise AudioError::DecodeFailed("Vorbis: not an audio packet")
  }
  // Mode number
  let mode_bits = ilog(info.modes.length() - 1)
  let mode_number = read_bits(br, mode_bits)
  if mode_number >= info.modes.length() {
    raise AudioError::DecodeFailed("Vorbis: invalid mode number")
  }
  let mode = info.modes[mode_number]
  let n = if mode.block_flag { info.blocksize_1 } else { info.blocksize_0 }
  let half_n = n / 2
  let mapping = info.mappings[mode.mapping]
  // Decode floors
  let floors_data : Array[Array[Float]?] = []
  let no_residue : Array[Bool] = []
  for ch in 0..<info.channels {
    let submap_idx = mapping.mux[ch]
    let floor_idx = mapping.submap_floor[submap_idx]
    if floor_idx < info.floors.length() {
      let floor_result = decode_floor1(
        br,
        info.floors[floor_idx],
        info.codebooks,
        half_n,
      )
      match floor_result {
        Some(curve) => {
          floors_data.push(Some(curve))
          no_residue.push(false)
        }
        None => {
          floors_data.push(None)
          no_residue.push(true)
        }
      }
    } else {
      floors_data.push(None)
      no_residue.push(true)
    }
  }
  // Decode residues
  let residue_output : Array[Array[Float]] = []
  for submap_idx in 0..<mapping.submaps {
    let residue_idx = mapping.submap_residue[submap_idx]
    if residue_idx < info.residues.length() {
      let do_not_decode : Array[Bool] = []
      let mut ch_count = 0
      for ch in 0..<info.channels {
        if mapping.mux[ch] == submap_idx {
          do_not_decode.push(no_residue[ch])
          ch_count = ch_count + 1
        }
      }
      if ch_count > 0 {
        let res_data = decode_residue(
          br,
          info.residues[residue_idx],
          info.codebooks,
          half_n,
          ch_count,
          do_not_decode,
        )
        for c in res_data {
          residue_output.push(c)
        }
      }
    }
  }
  // Coupling
  for step in (mapping.coupling_steps - 1)..<=0 {
    if step < 0 {
      break
    }
    let mag_ch = mapping.coupling_magnitude[step]
    let ang_ch = mapping.coupling_angle[step]
    if mag_ch < residue_output.length() && ang_ch < residue_output.length() {
      for i in 0..<half_n {
        let m = residue_output[mag_ch][i]
        let a = residue_output[ang_ch][i]
        let (new_m, new_a) = if m > 0.0 {
          if a > 0.0 {
            (m, m - a)
          } else {
            (m + a, m)
          }
        } else if a > 0.0 {
          (m, m + a)
        } else {
          (m - a, m)
        }
        residue_output[mag_ch][i] = new_m
        residue_output[ang_ch][i] = new_a
      }
    }
  }
  // Floor multiply + IMDCT + windowing per channel
  let left : Array[Float] = Array::make(n, (0.0 : Float))
  let right : Array[Float] = Array::make(n, (0.0 : Float))
  for ch in 0..<info.channels {
    let spectral = if ch < residue_output.length() {
      residue_output[ch]
    } else {
      Array::make(half_n, (0.0 : Float))
    }
    // Multiply with floor
    match floors_data[ch] {
      Some(floor_curve) =>
        for i in 0..<half_n {
          if i < floor_curve.length() {
            spectral[i] = spectral[i] * floor_curve[i]
          }
        }
      None =>
        for i in 0..<half_n {
          spectral[i] = 0.0
        }
    }
    // IMDCT
    let time_domain = imdct(spectral, n)
    // Apply window
    apply_vorbis_window(time_domain, n)
    // Store in output
    if ch == 0 {
      for i in 0..<n {
        left[i] = time_domain[i]
      }
    } else if ch == 1 {
      for i in 0..<n {
        right[i] = time_domain[i]
      }
    }
  }
  // For mono, duplicate to right channel
  if info.channels == 1 {
    for i in 0..<n {
      right[i] = left[i]
    }
  }
  (left, right)
}

///|
test "vorbis_window symmetry" {
  // Window should be symmetric: w(i) = w(n-1-i)
  let n = 64
  for i in 0..<(n / 2) {
    let w1 = vorbis_window(i, n)
    let w2 = vorbis_window(n - 1 - i, n)
    let diff = w1 - w2
    let abs_diff = if diff >= 0.0 { diff } else { -diff }
    assert_true(abs_diff < 0.001)
  }
}

///|
test "vorbis_window boundary values" {
  let n = 32
  // Window at edges should be near 0
  let w0 = vorbis_window(0, n)
  assert_true(w0 > 0.0 && w0 < 0.2)
  // Window at center should be near 1
  let w_mid = vorbis_window(n / 2, n)
  assert_true(w_mid > 0.8)
}

///|
test "apply_vorbis_window" {
  let data : Array[Float] = Array::make(8, (1.0 : Float))
  apply_vorbis_window(data, 8)
  // After windowing, edges should be attenuated
  assert_true(data[0] < 0.5)
  assert_true(data[7] < 0.5)
  // Middle should be closer to 1
  assert_true(data[3] > 0.5)
  assert_true(data[4] > 0.5)
}
