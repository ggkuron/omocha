///|
test "new_audio_runtime frames_per_tick" {
  let rt = @audio.new_audio_runtime(44100, 60)
  // 44100 / 60 = 735
  inspect(rt.frames_per_tick, content="735")
  inspect(rt.tick_count, content="0")
  inspect(rt.initialized, content="false")
}

///|
test "new_audio_runtime output buffer size" {
  let rt = @audio.new_audio_runtime(48000, 30)
  // 48000 / 30 = 1600 frames, stereo = 3200 samples
  inspect(rt.output_buffer.length(), content="3200")
}

///|
test "audio_tick calls backend hooks" {
  let call_log : Array[String] = []
  let hooks = @audio.AudioBackendHooks::{
    initialize: fn(sr, ch) { call_log.push("init(\{sr},\{ch})") },
    write: fn(_buf, frames) { call_log.push("write(\{frames})") },
    suspend: fn() { () },
    resume_playback: fn() { () },
    close: fn() { () },
    report_consumed: fn() { 0 },
  }
  @audio.set_audio_backend_hooks(hooks)
  let rt = @audio.new_audio_runtime(44100, 60)
  @audio.audio_tick(rt)
  inspect(rt.tick_count, content="1")
  inspect(rt.initialized, content="true")
  // Should have called initialize then write
  inspect(call_log, content="[\"init(44100,2)\", \"write(735)\"]")
  // Second tick should not re-initialize
  @audio.audio_tick(rt)
  inspect(rt.tick_count, content="2")
  inspect(
    call_log,
    content="[\"init(44100,2)\", \"write(735)\", \"write(735)\"]",
  )
  @audio.reset_audio_backend_hooks()
}

///|
test "play_sound and audio_tick integration" {
  let write_count = Ref::new(0)
  let hooks = @audio.AudioBackendHooks::{
    initialize: fn(_sr, _ch) { () },
    write: fn(_buf, _frames) { write_count.val = write_count.val + 1 },
    suspend: fn() { () },
    resume_playback: fn() { () },
    close: fn() { () },
    report_consumed: fn() { 0 },
  }
  @audio.set_audio_backend_hooks(hooks)
  let rt = @audio.new_audio_runtime(44100, 60)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  @audio.set_sample(buf, 0, 0, 0.5)
  let id = @audio.play_sound(rt, buf, gain=0.8)
  inspect(id, content="VoiceId(0)")
  inspect(rt.mixer.voices.length(), content="1")
  @audio.audio_tick(rt)
  inspect(write_count.val, content="1")
  @audio.reset_audio_backend_hooks()
}
