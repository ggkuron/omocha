///|
fn make_test_mixer_and_voice() -> (@audio.Mixer, @audio.VoiceId) {
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  for i in 0..<100 {
    @audio.set_sample(buf, i, 0, 0.5)
  }
  let mixer = @audio.new_mixer(44100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let id = @audio.add_voice(mixer, voice)
  (mixer, id)
}

///|
test "pause voice stops position advance" {
  let (mixer, id) = make_test_mixer_and_voice()
  let output = FixedArray::make(8, (0.0 : Float))
  // Tick to advance position
  @audio.tick(mixer, 4, output)
  let pos_before = @audio.voice_position(mixer, id)
  assert_true(pos_before is Some(_))
  // Pause
  assert_true(@audio.pause_voice(mixer, id))
  inspect(@audio.voice_state(mixer, id), content="Some(Paused)")
  // Tick again - position should not change
  @audio.tick(mixer, 4, output)
  let pos_after = @audio.voice_position(mixer, id)
  inspect(pos_before, content=pos_after.to_string())
}

///|
test "resume voice resumes playback" {
  let (mixer, id) = make_test_mixer_and_voice()
  let output = FixedArray::make(8, (0.0 : Float))
  @audio.tick(mixer, 4, output)
  @audio.pause_voice(mixer, id) |> ignore
  let pos_paused = @audio.voice_position(mixer, id)
  // Resume
  assert_true(@audio.resume_voice(mixer, id))
  inspect(@audio.voice_state(mixer, id), content="Some(Playing)")
  // Tick - position should advance
  @audio.tick(mixer, 4, output)
  let pos_after = @audio.voice_position(mixer, id)
  match (pos_paused, pos_after) {
    (Some(p), Some(a)) => assert_true(a > p)
    _ => assert_true(false)
  }
}

///|
test "stop voice -> Stopped" {
  let (mixer, id) = make_test_mixer_and_voice()
  assert_true(@audio.stop_voice(mixer, id))
  inspect(@audio.voice_state(mixer, id), content="Some(Stopped)")
  @audio.collect_stopped(mixer)
  inspect(mixer.voices.length(), content="0")
}

///|
test "seek voice sets position" {
  let (mixer, id) = make_test_mixer_and_voice()
  assert_true(@audio.seek_voice(mixer, id, 10))
  let tol : Float = 0.001
  match @audio.voice_position(mixer, id) {
    Some(pos) => assert_true(Float::is_close(pos, 10.0, absolute_tolerance=tol))
    None => assert_true(false)
  }
  // Tick should advance from position 10
  let output = FixedArray::make(4, (0.0 : Float))
  @audio.tick(mixer, 2, output)
  match @audio.voice_position(mixer, id) {
    Some(pos) => assert_true(Float::is_close(pos, 12.0, absolute_tolerance=tol))
    None => assert_true(false)
  }
}

///|
test "invalid VoiceId returns None/false" {
  let mixer = @audio.new_mixer(44100)
  let bad_id = @audio.VoiceId(999)
  assert_true(@audio.find_voice(mixer, bad_id) is None)
  assert_true(@audio.pause_voice(mixer, bad_id) == false)
  assert_true(@audio.resume_voice(mixer, bad_id) == false)
  assert_true(@audio.stop_voice(mixer, bad_id) == false)
  assert_true(@audio.seek_voice(mixer, bad_id, 0) == false)
  assert_true(@audio.voice_position(mixer, bad_id) is None)
  assert_true(@audio.voice_state(mixer, bad_id) is None)
}
