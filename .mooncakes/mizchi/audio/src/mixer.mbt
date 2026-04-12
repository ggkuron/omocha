///|
/// Create a new stereo mixer at the given sample rate.
pub fn new_mixer(
  sample_rate : Int,
  resample_quality? : ResampleQuality = Linear,
) -> Mixer {
  Mixer::{
    voices: [],
    master_gain: 1.0,
    channels: 2,
    sample_rate,
    resample_quality,
    next_voice_id: 0,
  }
}

///|
/// Create a new voice from an audio source.
pub fn new_voice(
  mixer : Mixer,
  source : AudioSource,
  gain? : Float = 1.0,
  pan? : Float = 0.0,
  looping? : Bool = false,
  envelope? : Envelope? = None,
) -> Voice {
  let id = VoiceId(mixer.next_voice_id)
  mixer.next_voice_id = mixer.next_voice_id + 1
  let sample_rate = source_sample_rate(source)
  Voice::{
    id,
    source,
    position: 0.0,
    gain,
    pan,
    state: VoiceState::Playing,
    looping,
    sample_rate,
    loop_start: 0,
    loop_end: 0,
    envelope,
    effects: [],
  }
}

///|
/// Create a new voice from an AudioBuffer (convenience wrapper).
pub fn new_voice_from_buffer(
  mixer : Mixer,
  buffer : AudioBuffer,
  gain? : Float = 1.0,
  pan? : Float = 0.0,
  looping? : Bool = false,
  envelope? : Envelope? = None,
) -> Voice {
  new_voice(
    mixer,
    AudioSource::Buffer(buffer),
    gain~,
    pan~,
    looping~,
    envelope~,
  )
}

///|
/// Add a voice to the mixer. Returns the voice ID.
pub fn add_voice(mixer : Mixer, voice : Voice) -> VoiceId {
  mixer.voices.push(voice)
  voice.id
}

///|
/// Compute constant-power pan gains for left and right channels.
/// pan: -1.0 (full left) to 1.0 (full right), 0.0 = center.
pub fn pan_gains(pan : Float) -> (Float, Float) {
  // Map pan [-1, 1] to angle [0, pi/2]
  let half_pi = Float::from_double(@math.PI / 2.0)
  let angle = (pan + 1.0) * 0.5 * half_pi
  let left = @math.cosf(angle)
  let right = @math.sinf(angle)
  (left, right)
}

///|
/// Mix all playing voices into the output buffer for the given number of frames.
/// Output is interleaved stereo (2 channels). Buffer must have length >= frames * 2.
pub fn tick(mixer : Mixer, frames : Int, output : FixedArray[Float]) -> Unit {
  // Zero the output buffer
  output.fill(0.0, end=frames * 2)
  for voice in mixer.voices {
    if voice.state != VoiceState::Playing {
      continue
    }
    let src = voice.source
    let src_channels = source_channels(src)
    let src_frames = source_frame_count(src)
    let (pan_l, pan_r) = pan_gains(voice.pan)
    let voice_gain = voice.gain
    let ratio = resample_ratio(voice.sample_rate, mixer.sample_rate)
    for f in 0..<frames {
      let int_pos = voice.position.to_int()
      let end_frame = if voice.loop_end > 0 {
        voice.loop_end
      } else {
        src_frames
      }
      if int_pos >= end_frame {
        if voice.looping {
          voice.position = Float::from_int(voice.loop_start)
        } else {
          voice.state = VoiceState::Stopped
          break
        }
      }
      let env_gain : Float = match voice.envelope {
        Some(env) => {
          let g = envelope_tick(env)
          if env.phase == EnvelopePhase::Done {
            voice.state = VoiceState::Stopped
            break
          }
          g
        }
        None => 1.0
      }
      let combined_gain = voice_gain * env_gain
      let raw_l = resample_frame(
        mixer.resample_quality,
        src,
        voice.position,
        0,
        src_frames,
      )
      let proc_l = effects_chain_process(voice.effects, raw_l)
      let sample_l = proc_l * combined_gain * pan_l
      let sample_r = if src_channels >= 2 {
        resample_frame(
          mixer.resample_quality,
          src,
          voice.position,
          1,
          src_frames,
        ) *
        combined_gain *
        pan_r
      } else {
        proc_l * combined_gain * pan_r
      }
      let out_idx = f * 2
      output[out_idx] = output[out_idx] + sample_l
      output[out_idx + 1] = output[out_idx + 1] + sample_r
      voice.position = voice.position + ratio
    }
  }
  // Apply master gain
  if mixer.master_gain != 1.0 {
    for i in 0..<(frames * 2) {
      output[i] = output[i] * mixer.master_gain
    }
  }
}

///|
/// Remove stopped voices from the mixer.
pub fn collect_stopped(mixer : Mixer) -> Unit {
  mixer.voices = mixer.voices.filter(fn(v) { v.state != VoiceState::Stopped })
}
