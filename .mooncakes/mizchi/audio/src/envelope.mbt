///|
/// Create a new ADSR envelope.
pub fn new_envelope(config : EnvelopeConfig, sample_rate : Int) -> Envelope {
  Envelope::{
    config,
    sample_rate,
    phase: EnvelopePhase::Attack,
    level: 0.0,
    phase_position: 0,
    release_start_level: 0.0,
  }
}

///|
/// Advance envelope by one sample and return the current gain level.
pub fn envelope_tick(env : Envelope) -> Float {
  match env.phase {
    Attack => {
      let attack_samples = (env.config.attack * Float::from_int(env.sample_rate)).to_int()
      if attack_samples <= 0 {
        env.level = 1.0
        env.phase = EnvelopePhase::Decay
        env.phase_position = 0
        return env.level
      }
      env.level = Float::from_int(env.phase_position) /
        Float::from_int(attack_samples)
      env.phase_position = env.phase_position + 1
      if env.phase_position >= attack_samples {
        env.level = 1.0
        env.phase = EnvelopePhase::Decay
        env.phase_position = 0
      }
      env.level
    }
    Decay => {
      let decay_samples = (env.config.decay * Float::from_int(env.sample_rate)).to_int()
      if decay_samples <= 0 {
        env.level = env.config.sustain
        env.phase = EnvelopePhase::Sustain
        env.phase_position = 0
        return env.level
      }
      let t = Float::from_int(env.phase_position) /
        Float::from_int(decay_samples)
      env.level = 1.0 + (env.config.sustain - 1.0) * t
      env.phase_position = env.phase_position + 1
      if env.phase_position >= decay_samples {
        env.level = env.config.sustain
        env.phase = EnvelopePhase::Sustain
        env.phase_position = 0
      }
      env.level
    }
    Sustain => {
      env.level = env.config.sustain
      env.level
    }
    Release => {
      let release_samples = (env.config.release_time *
      Float::from_int(env.sample_rate)).to_int()
      if release_samples <= 0 {
        env.level = 0.0
        env.phase = EnvelopePhase::Done
        return env.level
      }
      let t = Float::from_int(env.phase_position) /
        Float::from_int(release_samples)
      env.level = env.release_start_level * (1.0 - t)
      env.phase_position = env.phase_position + 1
      if env.phase_position >= release_samples {
        env.level = 0.0
        env.phase = EnvelopePhase::Done
      }
      if env.level < 0.0 {
        env.level = 0.0
      }
      env.level
    }
    Done => 0.0
  }
}

///|
/// Trigger the release phase of the envelope.
pub fn envelope_release(env : Envelope) -> Unit {
  match env.phase {
    Done => ()
    _ => {
      env.release_start_level = env.level
      env.phase = EnvelopePhase::Release
      env.phase_position = 0
    }
  }
}

///|
/// Reset the envelope to the beginning of the attack phase.
pub fn envelope_reset(env : Envelope) -> Unit {
  env.phase = EnvelopePhase::Attack
  env.level = 0.0
  env.phase_position = 0
  env.release_start_level = 0.0
}
