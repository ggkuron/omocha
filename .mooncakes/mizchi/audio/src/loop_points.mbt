///|
/// Create a new voice with loop points from an audio source.
pub fn new_voice_with_loop(
  mixer : Mixer,
  source : AudioSource,
  loop_start : Int,
  loop_end : Int,
  gain? : Float = 1.0,
  pan? : Float = 0.0,
  looping? : Bool = true,
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
    loop_start,
    loop_end,
    envelope: None,
    effects: [],
  }
}
