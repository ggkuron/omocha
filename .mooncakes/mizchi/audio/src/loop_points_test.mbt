///|
test "loop points: loop_start=10 loop_end=20 loops in range" {
  // Create a 100-frame mono buffer with ascending values
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  for i in 0..<100 {
    @audio.set_sample(buf, i, 0, Float::from_int(i))
  }
  let mixer = @audio.new_mixer(
    44100,
    resample_quality=@audio.ResampleQuality::Nearest,
  )
  let voice = @audio.new_voice_with_loop(
    mixer,
    @audio.AudioSource::Buffer(buf),
    10,
    20,
    looping=true,
  )
  let _ = @audio.add_voice(mixer, voice)
  // Tick enough frames to go past loop_end and verify it wraps
  let output = FixedArray::make(200, (0.0 : Float))
  // First tick: 25 frames to go past the loop point
  @audio.tick(mixer, 25, output)
  // Voice should still be playing (it looped)
  assert_true(voice.state == @audio.VoiceState::Playing)
  // Position should have wrapped back past loop_start
  assert_true(voice.position >= 10.0)
}

///|
test "loop points: loop_end=0 uses buffer end" {
  let buf = @audio.new_audio_buffer(1, 44100, 50)
  for i in 0..<50 {
    @audio.set_sample(buf, i, 0, Float::from_int(i))
  }
  let mixer = @audio.new_mixer(
    44100,
    resample_quality=@audio.ResampleQuality::Nearest,
  )
  let voice = @audio.new_voice_with_loop(
    mixer,
    @audio.AudioSource::Buffer(buf),
    0,
    0, // 0 = use buffer end
    looping=true,
  )
  let _ = @audio.add_voice(mixer, voice)
  let output = FixedArray::make(120, (0.0 : Float))
  @audio.tick(mixer, 60, output)
  // Should still be playing (looped at buffer end)
  assert_true(voice.state == @audio.VoiceState::Playing)
}

///|
test "loop points: non-looping stops at loop_end" {
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  for i in 0..<100 {
    @audio.set_sample(buf, i, 0, Float::from_int(i))
  }
  let mixer = @audio.new_mixer(
    44100,
    resample_quality=@audio.ResampleQuality::Nearest,
  )
  let voice = @audio.new_voice_with_loop(
    mixer,
    @audio.AudioSource::Buffer(buf),
    0,
    50,
    looping=false,
  )
  let _ = @audio.add_voice(mixer, voice)
  let output = FixedArray::make(200, (0.0 : Float))
  @audio.tick(mixer, 100, output)
  // Should have stopped at frame 50
  assert_true(voice.state == @audio.VoiceState::Stopped)
}
