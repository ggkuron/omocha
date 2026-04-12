///|
/// Commands for controlling the audio mixer from any thread.
pub(all) enum AudioCommand {
  PlaySource(AudioSource, Float, Float, Bool) // source, gain, pan, looping
  PauseVoice(VoiceId)
  ResumeVoice(VoiceId)
  StopVoice(VoiceId)
  SeekVoice(VoiceId, Int)
  SetVoiceGain(VoiceId, Float)
  SetVoicePan(VoiceId, Float)
  SetMasterGain(Float)
} derive(Show)

///|
/// Queue of audio commands to be flushed on the mixer thread.
pub(all) struct CommandQueue {
  mut commands : Array[AudioCommand]
  mut pending_ids : Array[VoiceId]
}

///|
/// Create a new empty command queue.
pub fn new_command_queue() -> CommandQueue {
  CommandQueue::{ commands: [], pending_ids: [] }
}

///|
/// Enqueue a command. For PlaySource, returns the pre-allocated VoiceId.
pub fn enqueue(
  queue : CommandQueue,
  cmd : AudioCommand,
  mixer : Mixer,
) -> VoiceId? {
  match cmd {
    PlaySource(_) => {
      let id = VoiceId(mixer.next_voice_id)
      mixer.next_voice_id = mixer.next_voice_id + 1
      queue.pending_ids.push(id)
      queue.commands.push(cmd)
      Some(id)
    }
    _ => {
      queue.commands.push(cmd)
      None
    }
  }
}

///|
/// Number of pending commands.
pub fn queue_length(queue : CommandQueue) -> Int {
  queue.commands.length()
}

///|
/// Flush all commands to the mixer. Processes in FIFO order.
pub fn flush_commands(queue : CommandQueue, mixer : Mixer) -> Unit {
  let mut id_idx = 0
  for cmd in queue.commands {
    match cmd {
      PlaySource(source, gain, pan, looping) => {
        let id = queue.pending_ids[id_idx]
        id_idx = id_idx + 1
        let voice = Voice::{
          id,
          source,
          position: 0.0,
          gain,
          pan,
          state: VoiceState::Playing,
          looping,
          sample_rate: source_sample_rate(source),
          loop_start: 0,
          loop_end: 0,
          envelope: None,
          effects: [],
        }
        mixer.voices.push(voice)
      }
      PauseVoice(id) => pause_voice(mixer, id) |> ignore
      ResumeVoice(id) => resume_voice(mixer, id) |> ignore
      StopVoice(id) => stop_voice(mixer, id) |> ignore
      SeekVoice(id, frame) => seek_voice(mixer, id, frame) |> ignore
      SetVoiceGain(id, gain) =>
        match find_voice(mixer, id) {
          Some(voice) => voice.gain = gain
          None => ()
        }
      SetVoicePan(id, pan) =>
        match find_voice(mixer, id) {
          Some(voice) => voice.pan = pan
          None => ()
        }
      SetMasterGain(gain) => mixer.master_gain = gain
    }
  }
  queue.commands = []
  queue.pending_ids = []
}
