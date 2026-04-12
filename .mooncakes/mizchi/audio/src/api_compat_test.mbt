///|
/// API compatibility tests: verify all public types and functions are accessible
/// from blackbox test context.

///|
test "compat: AudioBuffer construction and access" {
  let buf = @audio.new_audio_buffer(2, 44100, 10)
  assert_eq(@audio.frame_count(buf), 10)
  assert_true(@audio.duration_secs(buf) > 0.0)
  @audio.set_sample(buf, 0, 0, 0.5)
  let v = @audio.get_sample(buf, 0, 0)
  let tol : Float = 0.0001
  assert_true(Float::is_close(v, 0.5, absolute_tolerance=tol))
}

///|
test "compat: WAV decode round-trip" {
  let header : Array[Byte] = []
  // "RIFF"
  header.push(b'\x52')
  header.push(b'\x49')
  header.push(b'\x46')
  header.push(b'\x46')
  // File size - 8 = 40
  header.push(b'\x28')
  header.push(b'\x00')
  header.push(b'\x00')
  header.push(b'\x00')
  // "WAVE"
  header.push(b'\x57')
  header.push(b'\x41')
  header.push(b'\x56')
  header.push(b'\x45')
  // "fmt "
  header.push(b'\x66')
  header.push(b'\x6D')
  header.push(b'\x74')
  header.push(b'\x20')
  // fmt chunk size = 16
  header.push(b'\x10')
  header.push(b'\x00')
  header.push(b'\x00')
  header.push(b'\x00')
  // audio format = 1 (PCM)
  header.push(b'\x01')
  header.push(b'\x00')
  // channels = 1
  header.push(b'\x01')
  header.push(b'\x00')
  // sample rate = 44100
  header.push(b'\x44')
  header.push(b'\xAC')
  header.push(b'\x00')
  header.push(b'\x00')
  // byte rate = 88200
  header.push(b'\x88')
  header.push(b'\x58')
  header.push(b'\x01')
  header.push(b'\x00')
  // block align = 2
  header.push(b'\x02')
  header.push(b'\x00')
  // bits per sample = 16
  header.push(b'\x10')
  header.push(b'\x00')
  // "data"
  header.push(b'\x64')
  header.push(b'\x61')
  header.push(b'\x74')
  header.push(b'\x61')
  // data chunk size = 4
  header.push(b'\x04')
  header.push(b'\x00')
  header.push(b'\x00')
  header.push(b'\x00')
  // Sample 1: 0x3FFF ~ 0.5
  header.push(b'\xFF')
  header.push(b'\x3F')
  // Sample 2: 0x0000 = 0.0
  header.push(b'\x00')
  header.push(b'\x00')
  let wav_bytes = Bytes::from_array(header)
  let buf = @audio.decode_wav(wav_bytes)
  assert_eq(@audio.frame_count(buf), 2)
  let s0 = @audio.get_sample(buf, 0, 0)
  assert_true(s0 > 0.4 && s0 < 0.6)
}

///|
test "compat: Mixer and Voice lifecycle" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 4)
  @audio.set_sample(buf, 0, 0, 1.0)
  @audio.set_sample(buf, 1, 0, 0.5)
  @audio.set_sample(buf, 2, 0, -0.5)
  @audio.set_sample(buf, 3, 0, 0.0)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let id = @audio.add_voice(mixer, voice)
  // Tick enough frames to exhaust the buffer
  let output = FixedArray::make(20, (0.0 : Float))
  @audio.tick(mixer, 10, output)
  // Voice should be stopped after exhausting buffer
  assert_true(@audio.voice_state(mixer, id) == Some(@audio.Stopped))
  @audio.collect_stopped(mixer)
  assert_true(@audio.find_voice(mixer, id) is None)
}

///|
test "compat: Voice control functions" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let voice = @audio.new_voice_from_buffer(mixer, buf)
  let id = @audio.add_voice(mixer, voice)
  assert_true(@audio.pause_voice(mixer, id))
  let out = FixedArray::make(2, (0.0 : Float))
  @audio.tick(mixer, 1, out)
  assert_true(@audio.voice_state(mixer, id) == Some(@audio.Paused))
  assert_true(@audio.resume_voice(mixer, id))
  @audio.tick(mixer, 1, out)
  assert_true(@audio.voice_state(mixer, id) == Some(@audio.Playing))
  assert_true(@audio.stop_voice(mixer, id))
  @audio.tick(mixer, 1, out)
  assert_true(@audio.voice_state(mixer, id) == Some(@audio.Stopped))
}

///|
test "compat: pan_gains" {
  let (l, r) = @audio.pan_gains(0.0)
  let tol : Float = 0.001
  assert_true(Float::is_close(l, r, absolute_tolerance=tol))
}

///|
test "compat: Runtime integration" {
  let rt = @audio.new_audio_runtime(44100, 60)
  let buf = @audio.new_audio_buffer(1, 44100, 10)
  @audio.set_sample(buf, 0, 0, 1.0)
  let _id = @audio.play_sound(rt, buf)
  @audio.audio_tick(rt)
}

///|
test "compat: Effects chain" {
  let coeffs = @audio.lowpass_coefficients(1000.0, 44100)
  let _hp = @audio.highpass_coefficients(100.0, 44100)
  let out = @audio.biquad_process(coeffs, 1.0)
  assert_true(out > -2.0 && out < 2.0)
  let delay = @audio.new_delay(100.0, 44100, feedback=0.5, mix=0.5)
  let d_out = @audio.delay_process(delay, 1.0)
  assert_true(d_out > -2.0 && d_out < 2.0)
  let node = @audio.Lowpass(@audio.lowpass_coefficients(2000.0, 44100))
  let e_out = @audio.effect_process(node, 0.5)
  assert_true(e_out > -2.0 && e_out < 2.0)
  let chain : Array[@audio.EffectNode] = [
    @audio.Lowpass(@audio.lowpass_coefficients(3000.0, 44100)),
    @audio.Delay(@audio.new_delay(50.0, 44100, feedback=0.3, mix=0.3)),
  ]
  let c_out = @audio.effects_chain_process(chain, 0.8)
  assert_true(c_out > -2.0 && c_out < 2.0)
}

///|
test "compat: Envelope lifecycle" {
  let config = @audio.EnvelopeConfig::{
    attack: 0.01,
    decay: 0.01,
    sustain: 0.8,
    release_time: 0.05,
  }
  let env = @audio.new_envelope(config, 44100)
  let mut level = @audio.envelope_tick(env)
  assert_true(level >= 0.0)
  @audio.envelope_release(env)
  level = @audio.envelope_tick(env)
  assert_true(level >= 0.0)
  @audio.envelope_reset(env)
}

///|
test "compat: Cache operations" {
  let cache = @audio.new_asset_cache(1024 * 1024, 10)
  let buf = @audio.new_audio_buffer(1, 44100, 10)
  @audio.cache_put(cache, "test_sound", buf)
  let result = @audio.cache_get(cache, "test_sound")
  assert_true(result is Some(_))
  ignore(@audio.cache_evict(cache, "test_sound"))
  let result2 = @audio.cache_get(cache, "test_sound")
  assert_true(result2 is None)
  @audio.cache_clear(cache)
}

///|
test "compat: RingBuffer" {
  let rb = @audio.new_ring_buffer(16, 1)
  assert_eq(@audio.ring_available(rb), 0)
  assert_eq(@audio.ring_free(rb), 16)
  let data = FixedArray::make(4, (0.0 : Float))
  data[0] = 1.0
  data[1] = 2.0
  data[2] = 3.0
  data[3] = 4.0
  ignore(@audio.ring_write(rb, data, 4))
  assert_eq(@audio.ring_available(rb), 4)
  let v = @audio.ring_peek(rb, 0, 0)
  let tol : Float = 0.001
  assert_true(Float::is_close(v, 1.0, absolute_tolerance=tol))
  @audio.ring_consume(rb, 4)
  assert_eq(@audio.ring_available(rb), 0)
  @audio.ring_reset(rb)
}

///|
test "compat: StreamingSource" {
  let pull_fn : (FixedArray[Float], Int) -> Int = fn(buf, frames) {
    for i in 0..<frames {
      buf[i] = 0.5
    }
    frames
  }
  let ss = @audio.new_streaming_source(
    @audio.PullCallback(pull_fn),
    1,
    44100,
    buffer_frames=64,
  )
  @audio.streaming_fill(ss, 16)
  assert_true(@audio.streaming_available(ss) > 0)
}

///|
test "compat: AudioSource" {
  let buf = @audio.new_audio_buffer(1, 44100, 10)
  let src = @audio.Buffer(buf)
  assert_eq(@audio.source_frame_count(src), 10)
  assert_eq(@audio.source_sample_rate(src), 44100)
  assert_eq(@audio.source_channels(src), 1)
  let _ = @audio.read_source_sample(src, 0, 0)
}

///|
test "compat: CommandQueue" {
  let mixer = @audio.new_mixer(44100)
  let q = @audio.new_command_queue()
  let buf = @audio.new_audio_buffer(1, 44100, 10)
  let src = @audio.Buffer(buf)
  let _id = @audio.enqueue(q, @audio.PlaySource(src, 1.0, 0.0, false), mixer)
  assert_eq(@audio.queue_length(q), 1)
  @audio.flush_commands(q, mixer)
  assert_eq(@audio.queue_length(q), 0)
}

///|
test "compat: LatencyMonitor" {
  let mon = @audio.new_latency_monitor()
  @audio.update_latency(mon, 256, 260)
  let _ = @audio.drift_frames(mon)
  let _ = @audio.drift_secs(mon, 44100)
  let _ = @audio.underrun_count(mon)
  @audio.reset_latency_monitor(mon)
}

///|
test "compat: Backend hooks" {
  let _ = @audio.get_audio_backend_hooks()
  @audio.set_audio_backend_hooks(@audio.AudioBackendHooks::{
    initialize: fn(_sr, _ch) { () },
    write: fn(_buf, _frames) { () },
    suspend: fn() { () },
    resume_playback: fn() { () },
    close: fn() { () },
    report_consumed: fn() { 0 },
  })
  @audio.reset_audio_backend_hooks()
}

///|
test "compat: Voice with loop" {
  let mixer = @audio.new_mixer(44100)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  let src = @audio.Buffer(buf)
  let voice = @audio.new_voice_with_loop(mixer, src, 10, 50)
  let _id = @audio.add_voice(mixer, voice)
}

///|
test "compat: pub(all) types constructible" {
  let _buf = @audio.AudioBuffer::{
    channels: 1,
    sample_rate: 44100,
    data: FixedArray::make(10, (0.0 : Float)),
  }
  let _id = @audio.VoiceId(1)
  let _state = @audio.Playing
  let _quality = @audio.Linear
  let _config = @audio.EnvelopeConfig::{
    attack: 0.01,
    decay: 0.01,
    sustain: 0.8,
    release_time: 0.1,
  }
  let _phase = @audio.Attack
  let _policy = @audio.Normal
  let _cmd = @audio.SetMasterGain(0.5)
}
