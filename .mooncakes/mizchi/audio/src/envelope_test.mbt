///|
test "envelope attack ramp" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.01,
    decay: 0.0,
    sustain: 1.0,
    release_time: 0.0,
  }
  let env = @audio.new_envelope(config, 1000) // 10 samples for attack
  // First sample should be 0.0
  let v0 = @audio.envelope_tick(env)
  assert_true(v0 < 0.01)
  // Mid-attack should be around 0.5
  for _ in 1..<5 {
    @audio.envelope_tick(env) |> ignore
  }
  let v5 = @audio.envelope_tick(env)
  assert_true(v5 > 0.4 && v5 < 0.7)
  // After attack completes (10 samples), transitions to Decay
  for _ in 6..<10 {
    @audio.envelope_tick(env) |> ignore
  }
  // With decay=0, one more tick transitions Decay→Sustain
  @audio.envelope_tick(env) |> ignore
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  assert_true(env.level > 0.99)
}

///|
test "envelope decay ramp" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.0,
    decay: 0.01,
    sustain: 0.5,
    release_time: 0.0,
  }
  let env = @audio.new_envelope(config, 1000)
  // Attack is 0, should jump to decay immediately
  let v0 = @audio.envelope_tick(env)
  assert_true(v0 > 0.9) // starts at 1.0
  // Run through decay (10 samples)
  for _ in 1..<11 {
    @audio.envelope_tick(env) |> ignore
  }
  // Should reach sustain level
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  let v = @audio.envelope_tick(env)
  assert_true(v > 0.49 && v < 0.51)
}

///|
test "envelope sustain holds" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.0,
    decay: 0.0,
    sustain: 0.7,
    release_time: 0.0,
  }
  let env = @audio.new_envelope(config, 1000)
  // Should reach sustain quickly
  for _ in 0..<5 {
    @audio.envelope_tick(env) |> ignore
  }
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  // Sustain should hold
  let v1 = @audio.envelope_tick(env)
  let v2 = @audio.envelope_tick(env)
  let v3 = @audio.envelope_tick(env)
  assert_true(v1 > 0.69 && v1 < 0.71)
  assert_true(v2 > 0.69 && v2 < 0.71)
  assert_true(v3 > 0.69 && v3 < 0.71)
}

///|
test "envelope release ramp" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.0,
    decay: 0.0,
    sustain: 0.8,
    release_time: 0.01,
  }
  let env = @audio.new_envelope(config, 1000) // 10 samples for release
  // Reach sustain
  for _ in 0..<3 {
    @audio.envelope_tick(env) |> ignore
  }
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  // Trigger release
  @audio.envelope_release(env)
  assert_true(env.phase == @audio.EnvelopePhase::Release)
  // First release sample should be close to sustain level
  let v0 = @audio.envelope_tick(env)
  assert_true(v0 > 0.7)
  // Run through release
  for _ in 1..<10 {
    @audio.envelope_tick(env) |> ignore
  }
  // Should reach Done
  assert_true(env.phase == @audio.EnvelopePhase::Done)
  let v_done = @audio.envelope_tick(env)
  assert_true(v_done < 0.01)
}

///|
test "envelope done outputs zero" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.0,
    decay: 0.0,
    sustain: 1.0,
    release_time: 0.0,
  }
  let env = @audio.new_envelope(config, 1000)
  // Reach sustain
  @audio.envelope_tick(env) |> ignore
  @audio.envelope_tick(env) |> ignore
  // Trigger release with 0 release time → immediate Done
  @audio.envelope_release(env)
  let v = @audio.envelope_tick(env)
  assert_true(v < 0.01)
  assert_true(env.phase == @audio.EnvelopePhase::Done)
  // Done should keep outputting 0
  assert_true(@audio.envelope_tick(env) < 0.01)
  assert_true(@audio.envelope_tick(env) < 0.01)
}

///|
test "envelope reset" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.01,
    decay: 0.01,
    sustain: 0.5,
    release_time: 0.01,
  }
  let env = @audio.new_envelope(config, 1000)
  // Advance to sustain
  for _ in 0..<25 {
    @audio.envelope_tick(env) |> ignore
  }
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  // Reset should go back to attack
  @audio.envelope_reset(env)
  assert_true(env.phase == @audio.EnvelopePhase::Attack)
  assert_true(env.level < 0.01)
  assert_true(env.phase_position == 0)
}

///|
test "envelope full ADSR cycle" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.005,
    decay: 0.005,
    sustain: 0.6,
    release_time: 0.005,
  }
  let env = @audio.new_envelope(config, 1000) // 5 samples per phase
  // Attack: 5 samples, 0→1
  let mut prev : Float = -1.0
  for _ in 0..<5 {
    let v = @audio.envelope_tick(env)
    assert_true(v >= prev) // monotonically increasing
    prev = v
  }
  // Decay: 5 samples, 1→0.6
  for _ in 0..<5 {
    let v = @audio.envelope_tick(env)
    assert_true(v <= 1.01) // monotonically decreasing from 1.0
  }
  assert_true(env.phase == @audio.EnvelopePhase::Sustain)
  // Sustain: holds at 0.6
  let s = @audio.envelope_tick(env)
  assert_true(s > 0.59 && s < 0.61)
  // Release
  @audio.envelope_release(env)
  for _ in 0..<5 {
    let v = @audio.envelope_tick(env)
    assert_true(v >= 0.0)
  }
  assert_true(env.phase == @audio.EnvelopePhase::Done)
}

///|
test "envelope integrated with mixer" {
  // Create a buffer with constant 1.0 samples
  let buf = @audio.new_audio_buffer(1, 1000, 100)
  for i in 0..<100 {
    @audio.set_sample(buf, i, 0, 1.0)
  }
  let config = @audio.EnvelopeConfig::{
    attack: 0.01,
    decay: 0.0,
    sustain: 1.0,
    release_time: 0.01,
  }
  let env = @audio.new_envelope(config, 1000)
  let mixer = @audio.new_mixer(
    1000,
    resample_quality=@audio.ResampleQuality::Nearest,
  )
  let voice = @audio.new_voice_from_buffer(mixer, buf, envelope=Some(env))
  let _ = @audio.add_voice(mixer, voice)
  // During attack, output should ramp up
  let output = FixedArray::make(20, (0.0 : Float))
  @audio.tick(mixer, 5, output)
  // First sample should be near 0 (attack start)
  assert_true(output[0] < 0.2)
  // 5th sample should be louder
  assert_true(output[8] > output[0])
}
