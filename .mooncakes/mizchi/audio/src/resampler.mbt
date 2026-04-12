///|
/// Compute the resampling ratio (source_rate / output_rate).
/// Ratio > 1.0 means downsampling, < 1.0 means upsampling.
fn resample_ratio(source_rate : Int, output_rate : Int) -> Float {
  Float::from_int(source_rate) / Float::from_int(output_rate)
}

///|
/// Read a resampled frame from a source at a fractional position.
fn resample_frame(
  quality : ResampleQuality,
  source : AudioSource,
  position : Float,
  channel : Int,
  frame_count : Int,
) -> Float {
  match quality {
    Nearest => resample_nearest(source, position, channel, frame_count)
    Linear => resample_linear(source, position, channel, frame_count)
    Cubic => resample_cubic(source, position, channel, frame_count)
  }
}

///|
fn clamp_frame(frame : Int, frame_count : Int) -> Int {
  if frame < 0 {
    0
  } else if frame >= frame_count {
    frame_count - 1
  } else {
    frame
  }
}

///|
fn resample_nearest(
  source : AudioSource,
  position : Float,
  channel : Int,
  frame_count : Int,
) -> Float {
  let frame = clamp_frame((position + 0.5).to_int(), frame_count)
  read_source_sample(source, frame, channel)
}

///|
fn resample_linear(
  source : AudioSource,
  position : Float,
  channel : Int,
  frame_count : Int,
) -> Float {
  let floor_pos = position.to_int()
  let frac = position - Float::from_int(floor_pos)
  let i0 = clamp_frame(floor_pos, frame_count)
  let i1 = clamp_frame(floor_pos + 1, frame_count)
  let s0 = read_source_sample(source, i0, channel)
  let s1 = read_source_sample(source, i1, channel)
  s0 + (s1 - s0) * frac
}

///|
fn resample_cubic(
  source : AudioSource,
  position : Float,
  channel : Int,
  frame_count : Int,
) -> Float {
  let floor_pos = position.to_int()
  let frac = position - Float::from_int(floor_pos)
  let i0 = clamp_frame(floor_pos - 1, frame_count)
  let i1 = clamp_frame(floor_pos, frame_count)
  let i2 = clamp_frame(floor_pos + 1, frame_count)
  let i3 = clamp_frame(floor_pos + 2, frame_count)
  let s0 = read_source_sample(source, i0, channel)
  let s1 = read_source_sample(source, i1, channel)
  let s2 = read_source_sample(source, i2, channel)
  let s3 = read_source_sample(source, i3, channel)
  // Hermite interpolation
  let c0 = s1
  let c1 = (s2 - s0) * 0.5
  let c2 = s0 - s1 * 2.5 + s2 * 2.0 - s3 * 0.5
  let c3 = (s3 - s0) * 0.5 + (s1 - s2) * 1.5
  ((c3 * frac + c2) * frac + c1) * frac + c0
}

///|
test "resample_ratio identity" {
  let ratio = resample_ratio(44100, 44100)
  let tol : Float = 0.0001
  assert_true(Float::is_close(ratio, 1.0, absolute_tolerance=tol))
}

///|
test "resample_ratio upsample" {
  let ratio = resample_ratio(22050, 44100)
  let tol : Float = 0.0001
  assert_true(Float::is_close(ratio, 0.5, absolute_tolerance=tol))
}

///|
test "resample_ratio downsample" {
  let ratio = resample_ratio(88200, 44100)
  let tol : Float = 0.0001
  assert_true(Float::is_close(ratio, 2.0, absolute_tolerance=tol))
}

///|
test "resample identity (44100->44100) matches direct read" {
  let buf = new_audio_buffer(1, 44100, 4)
  set_sample(buf, 0, 0, 0.1)
  set_sample(buf, 1, 0, 0.4)
  set_sample(buf, 2, 0, 0.9)
  set_sample(buf, 3, 0, -0.2)
  let src = AudioSource::Buffer(buf)
  let tol : Float = 0.0001
  // At integer positions, Linear should match direct sample
  for i in 0..<4 {
    let pos = Float::from_int(i)
    let resampled = resample_frame(Linear, src, pos, 0, 4)
    let direct = read_source_sample(src, i, 0)
    assert_true(Float::is_close(resampled, direct, absolute_tolerance=tol))
  }
}

///|
test "resample linear upsample (22050->44100)" {
  // 2 frames [0.0, 1.0] upsampled 2x should give [0.0, 0.5, 1.0, 1.0(clamped)]
  let buf = new_audio_buffer(1, 22050, 2)
  set_sample(buf, 0, 0, 0.0)
  set_sample(buf, 1, 0, 1.0)
  let src = AudioSource::Buffer(buf)
  let tol : Float = 0.0001
  // position 0.0 -> sample 0.0
  let s0 = resample_frame(Linear, src, 0.0, 0, 2)
  assert_true(Float::is_close(s0, 0.0, absolute_tolerance=tol))
  // position 0.5 -> interpolated 0.5
  let s1 = resample_frame(Linear, src, 0.5, 0, 2)
  assert_true(Float::is_close(s1, 0.5, absolute_tolerance=tol))
  // position 1.0 -> sample 1.0
  let s2 = resample_frame(Linear, src, 1.0, 0, 2)
  assert_true(Float::is_close(s2, 1.0, absolute_tolerance=tol))
}

///|
test "resample boundary clamp no panic" {
  let buf = new_audio_buffer(1, 44100, 2)
  set_sample(buf, 0, 0, 0.5)
  set_sample(buf, 1, 0, 1.0)
  let src = AudioSource::Buffer(buf)
  // Should not panic even with out-of-range positions
  let _ = resample_frame(Nearest, src, -1.0, 0, 2)
  let _ = resample_frame(Linear, src, 10.0, 0, 2)
  let _ = resample_frame(Cubic, src, -2.0, 0, 2)
  let _ = resample_frame(Cubic, src, 100.0, 0, 2)
}

///|
test "resample nearest" {
  let buf = new_audio_buffer(1, 44100, 3)
  set_sample(buf, 0, 0, 0.0)
  set_sample(buf, 1, 0, 1.0)
  set_sample(buf, 2, 0, 0.5)
  let src = AudioSource::Buffer(buf)
  let tol : Float = 0.0001
  // position 0.3 -> nearest is frame 0
  let s0 = resample_frame(Nearest, src, 0.3, 0, 3)
  assert_true(Float::is_close(s0, 0.0, absolute_tolerance=tol))
  // position 0.6 -> nearest is frame 1
  let s1 = resample_frame(Nearest, src, 0.6, 0, 3)
  assert_true(Float::is_close(s1, 1.0, absolute_tolerance=tol))
}
