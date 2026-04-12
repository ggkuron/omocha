///|
/// Compute lowpass biquad filter coefficients (RBJ Audio EQ Cookbook).
pub fn lowpass_coefficients(
  cutoff_hz : Float,
  sample_rate : Int,
  q? : Float = 0.707,
) -> BiquadState {
  let pi = Float::from_double(@math.PI)
  let sr = Float::from_int(sample_rate)
  let two : Float = 2.0
  let w0 = two * pi * cutoff_hz / sr
  let cos_w0 = @math.cosf(w0)
  let sin_w0 = @math.sinf(w0)
  let alpha = sin_w0 / (two * q)
  let one : Float = 1.0
  let b0 = (one - cos_w0) / two
  let b1 = one - cos_w0
  let b2 = (one - cos_w0) / two
  let a0 = one + alpha
  let neg_two : Float = -2.0
  let a1 = neg_two * cos_w0
  let a2 = one - alpha
  BiquadState::{
    b0: b0 / a0,
    b1: b1 / a0,
    b2: b2 / a0,
    a1: a1 / a0,
    a2: a2 / a0,
    z1: 0.0,
    z2: 0.0,
  }
}

///|
/// Compute highpass biquad filter coefficients (RBJ Audio EQ Cookbook).
pub fn highpass_coefficients(
  cutoff_hz : Float,
  sample_rate : Int,
  q? : Float = 0.707,
) -> BiquadState {
  let pi = Float::from_double(@math.PI)
  let sr = Float::from_int(sample_rate)
  let two : Float = 2.0
  let w0 = two * pi * cutoff_hz / sr
  let cos_w0 = @math.cosf(w0)
  let sin_w0 = @math.sinf(w0)
  let alpha = sin_w0 / (two * q)
  let one : Float = 1.0
  let b0 = (one + cos_w0) / two
  let neg_one_plus_cos = -(one + cos_w0)
  let b1 = neg_one_plus_cos
  let b2 = (one + cos_w0) / two
  let a0 = one + alpha
  let neg_two : Float = -2.0
  let a1 = neg_two * cos_w0
  let a2 = one - alpha
  BiquadState::{
    b0: b0 / a0,
    b1: b1 / a0,
    b2: b2 / a0,
    a1: a1 / a0,
    a2: a2 / a0,
    z1: 0.0,
    z2: 0.0,
  }
}

///|
/// Process one sample through a biquad filter (Direct Form II Transposed).
pub fn biquad_process(state : BiquadState, input : Float) -> Float {
  let output = state.b0 * input + state.z1
  state.z1 = state.b1 * input - state.a1 * output + state.z2
  state.z2 = state.b2 * input - state.a2 * output
  output
}

///|
/// Create a new delay effect.
pub fn new_delay(
  delay_ms : Float,
  sample_rate : Int,
  feedback? : Float = 0.3,
  mix? : Float = 0.5,
) -> DelayState {
  let sr = Float::from_int(sample_rate)
  let thousand : Float = 1000.0
  let delay_samples = (delay_ms / thousand * sr).to_int()
  let buf_size = if delay_samples > 0 { delay_samples } else { 1 }
  DelayState::{
    buffer: FixedArray::make(buf_size, (0.0 : Float)),
    write_pos: 0,
    delay_samples: buf_size,
    feedback,
    mix,
  }
}

///|
/// Process one sample through the delay effect.
/// Ring buffer: write_pos always points to the oldest sample (delay_samples ago).
pub fn delay_process(state : DelayState, input : Float) -> Float {
  let delayed = state.buffer[state.write_pos]
  state.buffer[state.write_pos] = input + delayed * state.feedback
  state.write_pos = (state.write_pos + 1) % state.delay_samples
  let one : Float = 1.0
  input * (one - state.mix) + delayed * state.mix
}

///|
/// Process one sample through an effect node.
pub fn effect_process(node : EffectNode, sample : Float) -> Float {
  match node {
    Lowpass(state) => biquad_process(state, sample)
    Highpass(state) => biquad_process(state, sample)
    Delay(state) => delay_process(state, sample)
  }
}

///|
/// Process one sample through an entire effects chain.
pub fn effects_chain_process(
  effects : Array[EffectNode],
  sample : Float,
) -> Float {
  let mut result = sample
  for effect in effects {
    result = effect_process(effect, result)
  }
  result
}
