///|
/// High-level audio runtime that integrates the mixer with backend hooks.
pub(all) struct AudioRuntime {
  mixer : Mixer
  output_buffer : FixedArray[Float]
  frames_per_tick : Int
  mut tick_count : Int
  mut initialized : Bool
  command_queue : CommandQueue
  latency_monitor : LatencyMonitor
}

///|
/// Create a new audio runtime.
/// sample_rate: output sample rate (e.g. 44100)
/// tps: ticks per second (e.g. 60 for 60fps game loop)
pub fn new_audio_runtime(sample_rate : Int, tps : Int) -> AudioRuntime {
  let frames_per_tick = sample_rate / tps
  let buffer_size = frames_per_tick * 2 // stereo
  AudioRuntime::{
    mixer: new_mixer(sample_rate),
    output_buffer: FixedArray::make(buffer_size, 0.0),
    frames_per_tick,
    tick_count: 0,
    initialized: false,
    command_queue: new_command_queue(),
    latency_monitor: new_latency_monitor(),
  }
}

///|
/// Run one game tick: flush commands, mix audio, write to backend, track latency.
pub fn audio_tick(rt : AudioRuntime) -> Unit {
  if not(rt.initialized) {
    let hooks = get_audio_backend_hooks()
    (hooks.initialize)(rt.mixer.sample_rate, rt.mixer.channels)
    rt.initialized = true
  }
  // Flush pending commands
  flush_commands(rt.command_queue, rt.mixer)
  // Mix audio
  tick(rt.mixer, rt.frames_per_tick, rt.output_buffer)
  let hooks = get_audio_backend_hooks()
  (hooks.write)(rt.output_buffer, rt.frames_per_tick)
  // Track latency
  let consumed = (hooks.report_consumed)()
  update_latency(rt.latency_monitor, rt.frames_per_tick, consumed)
  rt.tick_count = rt.tick_count + 1
  collect_stopped(rt.mixer)
}

///|
/// Play a sound buffer through the runtime. Returns voice ID.
pub fn play_sound(
  rt : AudioRuntime,
  buffer : AudioBuffer,
  gain? : Float = 1.0,
  pan? : Float = 0.0,
  looping? : Bool = false,
) -> VoiceId {
  let voice = new_voice_from_buffer(rt.mixer, buffer, gain~, pan~, looping~)
  add_voice(rt.mixer, voice)
}
