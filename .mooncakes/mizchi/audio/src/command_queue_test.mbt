///|
test "PlaySource enqueue and flush" {
  let mixer = @audio.new_mixer(44100)
  let queue = @audio.new_command_queue()
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let id = @audio.enqueue(
    queue,
    @audio.PlaySource(@audio.AudioSource::Buffer(buf), 0.8, 0.0, false),
    mixer,
  )
  assert_true(id is Some(_))
  inspect(@audio.queue_length(queue), content="1")
  inspect(mixer.voices.length(), content="0")
  @audio.flush_commands(queue, mixer)
  inspect(mixer.voices.length(), content="1")
  inspect(@audio.queue_length(queue), content="0")
  // Verify voice properties
  let tol : Float = 0.001
  assert_true(
    Float::is_close(mixer.voices[0].gain, 0.8, absolute_tolerance=tol),
  )
}

///|
test "SetVoiceGain via command queue" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let id = @audio.add_voice(mixer, voice)
  let queue = @audio.new_command_queue()
  let _ = @audio.enqueue(queue, @audio.SetVoiceGain(id, 0.3), mixer)
  @audio.flush_commands(queue, mixer)
  let tol : Float = 0.001
  assert_true(
    Float::is_close(mixer.voices[0].gain, 0.3, absolute_tolerance=tol),
  )
}

///|
test "Pause + Resume via command queue" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let id = @audio.add_voice(mixer, voice)
  let queue = @audio.new_command_queue()
  let _ = @audio.enqueue(queue, @audio.PauseVoice(id), mixer)
  @audio.flush_commands(queue, mixer)
  inspect(@audio.voice_state(mixer, id), content="Some(Paused)")
  let _ = @audio.enqueue(queue, @audio.ResumeVoice(id), mixer)
  @audio.flush_commands(queue, mixer)
  inspect(@audio.voice_state(mixer, id), content="Some(Playing)")
}

///|
test "PlaySource then StopVoice same flush" {
  let mixer = @audio.new_mixer(44100)
  let queue = @audio.new_command_queue()
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let id = @audio.enqueue(
    queue,
    @audio.PlaySource(@audio.AudioSource::Buffer(buf), 1.0, 0.0, false),
    mixer,
  )
  match id {
    Some(vid) => {
      let _ = @audio.enqueue(queue, @audio.StopVoice(vid), mixer)
    }
    None => assert_true(false)
  }
  @audio.flush_commands(queue, mixer)
  inspect(mixer.voices.length(), content="1")
  inspect(mixer.voices[0].state, content="Stopped")
}

///|
test "empty flush is no-op" {
  let mixer = @audio.new_mixer(44100)
  let queue = @audio.new_command_queue()
  @audio.flush_commands(queue, mixer)
  inspect(mixer.voices.length(), content="0")
}

///|
test "SetMasterGain via command queue" {
  let mixer = @audio.new_mixer(44100)
  let queue = @audio.new_command_queue()
  let _ = @audio.enqueue(queue, @audio.SetMasterGain(0.5), mixer)
  @audio.flush_commands(queue, mixer)
  let tol : Float = 0.001
  assert_true(Float::is_close(mixer.master_gain, 0.5, absolute_tolerance=tol))
}
