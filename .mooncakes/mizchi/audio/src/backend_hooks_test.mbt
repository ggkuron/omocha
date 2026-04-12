///|
test "default hooks are no-op" {
  @audio.reset_audio_backend_hooks()
  let hooks = @audio.get_audio_backend_hooks()
  // Should not panic
  (hooks.initialize)(44100, 2)
  let buf : FixedArray[Float] = FixedArray::make(4, 0.0)
  (hooks.write)(buf, 2)
  (hooks.suspend)()
  (hooks.resume_playback)()
  (hooks.close)()
  inspect((hooks.report_consumed)(), content="0")
}

///|
test "set_audio_backend_hooks injects custom hook" {
  let call_log : Array[String] = []
  let hooks = @audio.AudioBackendHooks::{
    initialize: fn(sr, ch) { call_log.push("init(\{sr},\{ch})") },
    write: fn(_buf, frames) { call_log.push("write(\{frames})") },
    suspend: fn() { call_log.push("suspend") },
    resume_playback: fn() { call_log.push("resume_playback") },
    close: fn() { call_log.push("close") },
    report_consumed: fn() { 42 },
  }
  @audio.set_audio_backend_hooks(hooks)
  let h = @audio.get_audio_backend_hooks()
  (h.initialize)(48000, 2)
  let buf : FixedArray[Float] = FixedArray::make(8, 0.0)
  (h.write)(buf, 4)
  (h.suspend)()
  (h.resume_playback)()
  (h.close)()
  inspect(
    call_log,
    content="[\"init(48000,2)\", \"write(4)\", \"suspend\", \"resume_playback\", \"close\"]",
  )
  inspect((h.report_consumed)(), content="42")
  // Cleanup
  @audio.reset_audio_backend_hooks()
}

///|
test "reset_audio_backend_hooks restores defaults" {
  let called = Ref::new(false)
  let hooks = @audio.AudioBackendHooks::{
    initialize: fn(_sr, _ch) { called.val = true },
    write: fn(_buf, _frames) { () },
    suspend: fn() { () },
    resume_playback: fn() { () },
    close: fn() { () },
    report_consumed: fn() { 0 },
  }
  @audio.set_audio_backend_hooks(hooks)
  @audio.reset_audio_backend_hooks()
  let h = @audio.get_audio_backend_hooks()
  (h.initialize)(44100, 2)
  // The custom hook should not have been called after reset
  assert_true(called.val == false)
}
