///|
test "new_audio_buffer mono" {
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  inspect(buf.channels, content="1")
  inspect(buf.sample_rate, content="44100")
  inspect(buf.data.length(), content="100")
}

///|
test "new_audio_buffer stereo" {
  let buf = @audio.new_audio_buffer(2, 48000, 50)
  inspect(buf.channels, content="2")
  inspect(buf.sample_rate, content="48000")
  inspect(buf.data.length(), content="100")
}

///|
test "frame_count" {
  let mono = @audio.new_audio_buffer(1, 44100, 100)
  inspect(@audio.frame_count(mono), content="100")
  let stereo = @audio.new_audio_buffer(2, 44100, 100)
  inspect(@audio.frame_count(stereo), content="100")
}

///|
test "duration_secs" {
  let buf = @audio.new_audio_buffer(2, 44100, 44100)
  // 44100 frames / 44100 Hz = 1.0 sec
  assert_true(@audio.duration_secs(buf) == 1.0)
}

///|
test "duration_secs half second" {
  let buf = @audio.new_audio_buffer(1, 48000, 24000)
  assert_true(@audio.duration_secs(buf) == 0.5)
}

///|
test "get_sample and set_sample mono" {
  let buf = @audio.new_audio_buffer(1, 44100, 4)
  @audio.set_sample(buf, 0, 0, 0.5)
  @audio.set_sample(buf, 1, 0, -0.25)
  @audio.set_sample(buf, 3, 0, 1.0)
  assert_true(@audio.get_sample(buf, 0, 0) == 0.5)
  assert_true(@audio.get_sample(buf, 1, 0) == -0.25)
  assert_true(@audio.get_sample(buf, 2, 0) == 0.0)
  assert_true(@audio.get_sample(buf, 3, 0) == 1.0)
}

///|
test "get_sample and set_sample stereo" {
  let buf = @audio.new_audio_buffer(2, 44100, 3)
  // frame 0: L=0.1, R=0.2
  @audio.set_sample(buf, 0, 0, 0.1)
  @audio.set_sample(buf, 0, 1, 0.2)
  // frame 1: L=0.3, R=0.4
  @audio.set_sample(buf, 1, 0, 0.3)
  @audio.set_sample(buf, 1, 1, 0.4)
  assert_true(@audio.get_sample(buf, 0, 0) == 0.1)
  assert_true(@audio.get_sample(buf, 0, 1) == 0.2)
  assert_true(@audio.get_sample(buf, 1, 0) == 0.3)
  assert_true(@audio.get_sample(buf, 1, 1) == 0.4)
  // frame 2 should be zero
  assert_true(@audio.get_sample(buf, 2, 0) == 0.0)
  assert_true(@audio.get_sample(buf, 2, 1) == 0.0)
}
