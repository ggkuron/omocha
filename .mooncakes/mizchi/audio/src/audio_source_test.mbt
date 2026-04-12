///|
test "source_frame_count for Buffer" {
  let buf = @audio.new_audio_buffer(2, 44100, 100)
  let src = @audio.AudioSource::Buffer(buf)
  inspect(@audio.source_frame_count(src), content="100")
}

///|
test "source_sample_rate for Buffer" {
  let buf = @audio.new_audio_buffer(1, 48000, 10)
  let src = @audio.AudioSource::Buffer(buf)
  inspect(@audio.source_sample_rate(src), content="48000")
}

///|
test "source_channels for Buffer" {
  let buf = @audio.new_audio_buffer(2, 44100, 10)
  let src = @audio.AudioSource::Buffer(buf)
  inspect(@audio.source_channels(src), content="2")
}

///|
test "read_source_sample from Buffer" {
  let buf = @audio.new_audio_buffer(1, 44100, 3)
  @audio.set_sample(buf, 0, 0, 0.5)
  @audio.set_sample(buf, 1, 0, -0.3)
  @audio.set_sample(buf, 2, 0, 1.0)
  let src = @audio.AudioSource::Buffer(buf)
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(
      @audio.read_source_sample(src, 0, 0),
      0.5,
      absolute_tolerance=tol,
    ),
  )
  assert_true(
    Float::is_close(
      @audio.read_source_sample(src, 1, 0),
      -0.3,
      absolute_tolerance=tol,
    ),
  )
  assert_true(
    Float::is_close(
      @audio.read_source_sample(src, 2, 0),
      1.0,
      absolute_tolerance=tol,
    ),
  )
}

///|
test "read_source_sample out of bounds returns 0" {
  let buf = @audio.new_audio_buffer(1, 44100, 2)
  @audio.set_sample(buf, 0, 0, 1.0)
  let src = @audio.AudioSource::Buffer(buf)
  assert_true(@audio.read_source_sample(src, -1, 0) == (0.0 : Float))
  assert_true(@audio.read_source_sample(src, 5, 0) == (0.0 : Float))
}

///|
test "VoiceId uniqueness" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 10)
  let v1 = @audio.new_voice_from_buffer(mixer, buf)
  let v2 = @audio.new_voice_from_buffer(mixer, buf)
  let v3 = @audio.new_voice_from_buffer(mixer, buf)
  assert_true(v1.id != v2.id)
  assert_true(v2.id != v3.id)
  assert_true(v1.id != v3.id)
}
