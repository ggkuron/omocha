///|
/// Backend hooks for platform-specific audio output.
pub(all) struct AudioBackendHooks {
  initialize : (Int, Int) -> Unit // (sample_rate, channels)
  write : (FixedArray[Float], Int) -> Unit // (buffer, frames)
  suspend : () -> Unit
  resume_playback : () -> Unit
  close : () -> Unit
  report_consumed : () -> Int // returns frames consumed by backend
}

///|
fn noop_initialize(_sample_rate : Int, _channels : Int) -> Unit {
  ()
}

///|
fn noop_write(_buf : FixedArray[Float], _frames : Int) -> Unit {
  ()
}

///|
fn noop_unit() -> Unit {
  ()
}

///|
fn noop_report_consumed() -> Int {
  0
}

///|
fn default_hooks() -> AudioBackendHooks {
  AudioBackendHooks::{
    initialize: noop_initialize,
    write: noop_write,
    suspend: noop_unit,
    resume_playback: noop_unit,
    close: noop_unit,
    report_consumed: noop_report_consumed,
  }
}

///|
let audio_backend_hooks : Ref[AudioBackendHooks] = Ref::new(default_hooks())

///|
/// Set the audio backend hooks.
pub fn set_audio_backend_hooks(hooks : AudioBackendHooks) -> Unit {
  audio_backend_hooks.val = hooks
}

///|
/// Reset hooks to no-op defaults.
pub fn reset_audio_backend_hooks() -> Unit {
  audio_backend_hooks.val = default_hooks()
}

///|
/// Get a reference to the current backend hooks.
pub fn get_audio_backend_hooks() -> AudioBackendHooks {
  audio_backend_hooks.val
}
