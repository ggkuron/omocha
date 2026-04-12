///|
test "pan_gains center" {
  let (l, r) = @audio.pan_gains(0.0)
  assert_true(Float::is_close(l, 0.7071, absolute_tolerance=0.001))
  assert_true(Float::is_close(r, 0.7071, absolute_tolerance=0.001))
}

///|
test "pan_gains full left" {
  let (l, r) = @audio.pan_gains(-1.0)
  assert_true(Float::is_close(l, 1.0, absolute_tolerance=0.001))
  assert_true(Float::is_close(r, 0.0, absolute_tolerance=0.001))
}

///|
test "pan_gains full right" {
  let (l, r) = @audio.pan_gains(1.0)
  assert_true(Float::is_close(l, 0.0, absolute_tolerance=0.001))
  assert_true(Float::is_close(r, 1.0, absolute_tolerance=0.001))
}

///|
fn make_float_buf(size : Int) -> FixedArray[Float] {
  FixedArray::make(size, 0.0)
}

///|
test "tick silence (no voices)" {
  let mixer = @audio.new_mixer(44100)
  let output = make_float_buf(8)
  @audio.tick(mixer, 4, output)
  for i in 0..<8 {
    assert_true(output[i] == (0.0 : Float))
  }
}

///|
test "tick single mono voice center" {
  let buf = @audio.new_audio_buffer(1, 44100, 4)
  @audio.set_sample(buf, 0, 0, 1.0)
  @audio.set_sample(buf, 1, 0, 0.5)
  @audio.set_sample(buf, 2, 0, -0.5)
  @audio.set_sample(buf, 3, 0, 0.0)
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let _ = @audio.add_voice(mixer, voice)
  let output = make_float_buf(8)
  @audio.tick(mixer, 4, output)
  let tol : Float = 0.001
  assert_true(Float::is_close(output[0], 0.7071, absolute_tolerance=tol))
  assert_true(Float::is_close(output[1], 0.7071, absolute_tolerance=tol))
  assert_true(Float::is_close(output[2], 0.3536, absolute_tolerance=tol))
  assert_true(Float::is_close(output[3], 0.3536, absolute_tolerance=tol))
}

///|
test "tick with gain" {
  let buf = @audio.new_audio_buffer(1, 44100, 1)
  @audio.set_sample(buf, 0, 0, 1.0)
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf, gain=0.5)
  let _ = @audio.add_voice(mixer, voice)
  let output = make_float_buf(2)
  @audio.tick(mixer, 1, output)
  let tol : Float = 0.001
  assert_true(Float::is_close(output[0], 0.3536, absolute_tolerance=tol))
  assert_true(Float::is_close(output[1], 0.3536, absolute_tolerance=tol))
}

///|
test "tick voice end -> Stopped" {
  let buf = @audio.new_audio_buffer(1, 44100, 2)
  @audio.set_sample(buf, 0, 0, 1.0)
  @audio.set_sample(buf, 1, 0, 1.0)
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let _ = @audio.add_voice(mixer, voice)
  let output = make_float_buf(8)
  @audio.tick(mixer, 4, output)
  inspect(voice.state, content="Stopped")
  assert_true(output[4] == (0.0 : Float))
  assert_true(output[5] == (0.0 : Float))
  assert_true(output[6] == (0.0 : Float))
  assert_true(output[7] == (0.0 : Float))
}

///|
test "tick looping voice" {
  let buf = @audio.new_audio_buffer(1, 44100, 2)
  @audio.set_sample(buf, 0, 0, 1.0)
  @audio.set_sample(buf, 1, 0, 0.5)
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf, looping=true)
  let _ = @audio.add_voice(mixer, voice)
  let output = make_float_buf(8)
  @audio.tick(mixer, 4, output)
  inspect(voice.state, content="Playing")
  let tol : Float = 0.001
  assert_true(Float::is_close(output[0], 0.7071, absolute_tolerance=tol))
  assert_true(Float::is_close(output[4], 0.7071, absolute_tolerance=tol))
  assert_true(Float::is_close(output[2], 0.3536, absolute_tolerance=tol))
  assert_true(Float::is_close(output[6], 0.3536, absolute_tolerance=tol))
}

///|
test "tick multi-voice summation" {
  let buf1 = @audio.new_audio_buffer(1, 44100, 1)
  @audio.set_sample(buf1, 0, 0, 0.5)
  let buf2 = @audio.new_audio_buffer(1, 44100, 1)
  @audio.set_sample(buf2, 0, 0, 0.3)
  let mixer = @audio.new_mixer(44100)
  let _ = @audio.add_voice(mixer, @audio.new_voice_from_buffer(mixer, buf1))
  let _ = @audio.add_voice(mixer, @audio.new_voice_from_buffer(mixer, buf2))
  let output = make_float_buf(2)
  @audio.tick(mixer, 1, output)
  let tol : Float = 0.001
  let expected : Float = (0.5 : Float) + (0.3 : Float)
  let expected_l : Float = expected * (0.7071 : Float)
  assert_true(Float::is_close(output[0], expected_l, absolute_tolerance=tol))
  assert_true(Float::is_close(output[1], expected_l, absolute_tolerance=tol))
}

///|
test "tick master_gain" {
  let buf = @audio.new_audio_buffer(1, 44100, 1)
  @audio.set_sample(buf, 0, 0, 1.0)
  let mixer = @audio.new_mixer(44100)
  mixer.master_gain = 0.5
  let _ = @audio.add_voice(mixer, @audio.new_voice_from_buffer(mixer, buf))
  let output = make_float_buf(2)
  @audio.tick(mixer, 1, output)
  let tol : Float = 0.001
  assert_true(Float::is_close(output[0], 0.3536, absolute_tolerance=tol))
  assert_true(Float::is_close(output[1], 0.3536, absolute_tolerance=tol))
}

///|
test "collect_stopped" {
  let buf = @audio.new_audio_buffer(1, 44100, 1)
  @audio.set_sample(buf, 0, 0, 1.0)
  let mixer = @audio.new_mixer(44100)
  let _ = @audio.add_voice(mixer, @audio.new_voice_from_buffer(mixer, buf))
  let _ = @audio.add_voice(mixer, @audio.new_voice_from_buffer(mixer, buf))
  let output = make_float_buf(4)
  @audio.tick(mixer, 2, output)
  inspect(mixer.voices.length(), content="2")
  @audio.collect_stopped(mixer)
  inspect(mixer.voices.length(), content="0")
}
