///|
test "mixer tick with resampled voice (identity)" {
  // Same sample rate: resampling should not change output
  let buf = @audio.new_audio_buffer(1, 44100, 2)
  @audio.set_sample(buf, 0, 0, 1.0)
  @audio.set_sample(buf, 1, 0, 0.5)
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let _ = @audio.add_voice(mixer, voice)
  let output = FixedArray::make(4, (0.0 : Float))
  @audio.tick(mixer, 2, output)
  let tol : Float = 0.001
  assert_true(Float::is_close(output[0], 0.7071, absolute_tolerance=tol))
  assert_true(Float::is_close(output[2], 0.3536, absolute_tolerance=tol))
}
