///|
test "AudioBuffer construction" {
  let buf = @audio.AudioBuffer::{
    channels: 2,
    sample_rate: 44100,
    data: FixedArray::make(4, 0.0),
  }
  inspect(buf.channels, content="2")
  inspect(buf.sample_rate, content="44100")
  inspect(buf.data.length(), content="4")
}

///|
test "VoiceState enum" {
  inspect(@audio.VoiceState::Playing, content="Playing")
  inspect(@audio.VoiceState::Paused, content="Paused")
  inspect(@audio.VoiceState::Stopped, content="Stopped")
  assert_true(@audio.VoiceState::Playing == @audio.VoiceState::Playing)
  assert_true(@audio.VoiceState::Playing != @audio.VoiceState::Stopped)
}
