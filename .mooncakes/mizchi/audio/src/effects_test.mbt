///|
test "lowpass filter attenuates high frequencies" {
  let state = @audio.lowpass_coefficients(100.0, 44100)
  // Feed a high-frequency signal (alternating +1/-1 = Nyquist/2)
  let mut max_output : Float = 0.0
  for _ in 0..<100 {
    let v1 = @audio.biquad_process(state, 1.0)
    let v2 = @audio.biquad_process(state, -1.0)
    let a1 = if v1 >= 0.0 { v1 } else { -v1 }
    let a2 = if v2 >= 0.0 { v2 } else { -v2 }
    if a1 > max_output {
      max_output = a1
    }
    if a2 > max_output {
      max_output = a2
    }
  }
  // High frequency should be heavily attenuated by 100Hz lowpass
  assert_true(max_output < 0.1)
}

///|
test "lowpass filter passes low frequencies" {
  let state = @audio.lowpass_coefficients(10000.0, 44100)
  // Feed DC signal (all 1.0) - should pass through
  let mut last : Float = 0.0
  for _ in 0..<200 {
    last = @audio.biquad_process(state, 1.0)
  }
  // DC should pass through nearly unchanged after settling
  assert_true(last > 0.9)
}

///|
test "highpass filter attenuates low frequencies" {
  let state = @audio.highpass_coefficients(10000.0, 44100)
  // Feed DC signal - should be attenuated
  let mut last : Float = 0.0
  for _ in 0..<200 {
    last = @audio.biquad_process(state, 1.0)
  }
  // DC should be attenuated by highpass
  assert_true(last < 0.1)
}

///|
test "highpass filter passes high frequencies" {
  let state = @audio.highpass_coefficients(100.0, 44100)
  // Feed a high-frequency signal
  let mut max_output : Float = 0.0
  for _ in 0..<200 {
    let v1 = @audio.biquad_process(state, 1.0)
    let v2 = @audio.biquad_process(state, -1.0)
    let a1 = if v1 >= 0.0 { v1 } else { -v1 }
    let a2 = if v2 >= 0.0 { v2 } else { -v2 }
    if a1 > max_output {
      max_output = a1
    }
    if a2 > max_output {
      max_output = a2
    }
  }
  // High frequency should pass through mostly
  assert_true(max_output > 0.8)
}

///|
test "delay effect produces delayed output" {
  // 10ms delay at 1000Hz = 10 sample delay
  let state = @audio.new_delay(10.0, 1000, feedback=0.0, mix=1.0)
  // Feed impulse
  let v0 = @audio.delay_process(state, 1.0)
  // First output should be 0 (wet only, no delayed signal yet)
  assert_true(v0 < 0.01)
  // Feed silence
  for _ in 0..<9 {
    @audio.delay_process(state, 0.0) |> ignore
  }
  // 10th output should have the delayed impulse
  let v10 = @audio.delay_process(state, 0.0)
  assert_true(v10 > 0.9)
}

///|
test "delay effect dry/wet mix" {
  let state = @audio.new_delay(10.0, 1000, feedback=0.0, mix=0.5)
  // Feed constant signal
  let v0 = @audio.delay_process(state, 1.0)
  // With mix=0.5: dry*0.5 + wet*0.5. wet=0 initially, so output = 0.5
  assert_true(v0 > 0.45 && v0 < 0.55)
}

///|
test "effects chain process passes through empty chain" {
  let effects : Array[@audio.EffectNode] = []
  let result = @audio.effects_chain_process(effects, 0.75)
  assert_true(result > 0.74 && result < 0.76)
}

///|
test "effects chain with lowpass" {
  let bq = @audio.lowpass_coefficients(100.0, 44100)
  let effects : Array[@audio.EffectNode] = [@audio.EffectNode::Lowpass(bq)]
  // High-frequency signal should be attenuated
  let mut max_out : Float = 0.0
  for _ in 0..<100 {
    let v1 = @audio.effects_chain_process(effects, 1.0)
    let v2 = @audio.effects_chain_process(effects, -1.0)
    let a1 = if v1 >= 0.0 { v1 } else { -v1 }
    let a2 = if v2 >= 0.0 { v2 } else { -v2 }
    if a1 > max_out {
      max_out = a1
    }
    if a2 > max_out {
      max_out = a2
    }
  }
  assert_true(max_out < 0.1)
}

///|
test "effect_process dispatches correctly" {
  let bq = @audio.lowpass_coefficients(10000.0, 44100)
  let node = @audio.EffectNode::Lowpass(bq)
  // DC should pass through lowpass with high cutoff
  let mut last : Float = 0.0
  for _ in 0..<100 {
    last = @audio.effect_process(node, 1.0)
  }
  assert_true(last > 0.9)
}
