///|
/// Find a voice by its ID. Returns None if not found.
pub fn find_voice(mixer : Mixer, id : VoiceId) -> Voice? {
  for voice in mixer.voices {
    if voice.id == id {
      return Some(voice)
    }
  }
  None
}

///|
/// Pause a playing voice. Returns true if state changed.
pub fn pause_voice(mixer : Mixer, id : VoiceId) -> Bool {
  match find_voice(mixer, id) {
    Some(voice) =>
      if voice.state == VoiceState::Playing {
        voice.state = VoiceState::Paused
        true
      } else {
        false
      }
    None => false
  }
}

///|
/// Resume a paused voice. Returns true if state changed.
pub fn resume_voice(mixer : Mixer, id : VoiceId) -> Bool {
  match find_voice(mixer, id) {
    Some(voice) =>
      if voice.state == VoiceState::Paused {
        voice.state = VoiceState::Playing
        true
      } else {
        false
      }
    None => false
  }
}

///|
/// Stop a voice. Returns true if voice was found.
pub fn stop_voice(mixer : Mixer, id : VoiceId) -> Bool {
  match find_voice(mixer, id) {
    Some(voice) => {
      voice.state = VoiceState::Stopped
      true
    }
    None => false
  }
}

///|
/// Seek a voice to a given frame position. Returns true if voice was found.
pub fn seek_voice(mixer : Mixer, id : VoiceId, frame : Int) -> Bool {
  match find_voice(mixer, id) {
    Some(voice) => {
      voice.position = Float::from_int(frame)
      true
    }
    None => false
  }
}

///|
/// Get the current position of a voice in frames.
pub fn voice_position(mixer : Mixer, id : VoiceId) -> Float? {
  match find_voice(mixer, id) {
    Some(voice) => Some(voice.position)
    None => None
  }
}

///|
/// Get the current state of a voice.
pub fn voice_state(mixer : Mixer, id : VoiceId) -> VoiceState? {
  match find_voice(mixer, id) {
    Some(voice) => Some(voice.state)
    None => None
  }
}
