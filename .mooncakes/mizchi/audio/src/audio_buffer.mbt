///|
/// Create a new AudioBuffer with zeroed samples.
pub fn new_audio_buffer(
  channels : Int,
  sample_rate : Int,
  frames : Int,
) -> AudioBuffer {
  AudioBuffer::{
    channels,
    sample_rate,
    data: FixedArray::make(frames * channels, 0.0),
  }
}

///|
/// Number of frames in the buffer.
pub fn frame_count(buf : AudioBuffer) -> Int {
  buf.data.length() / buf.channels
}

///|
/// Duration of the buffer in seconds.
pub fn duration_secs(buf : AudioBuffer) -> Float {
  Float::from_int(frame_count(buf)) / Float::from_int(buf.sample_rate)
}

///|
/// Get sample value at (frame, channel). Interleaved layout.
pub fn get_sample(buf : AudioBuffer, frame : Int, channel : Int) -> Float {
  buf.data[frame * buf.channels + channel]
}

///|
/// Set sample value at (frame, channel). Interleaved layout.
pub fn set_sample(
  buf : AudioBuffer,
  frame : Int,
  channel : Int,
  value : Float,
) -> Unit {
  buf.data[frame * buf.channels + channel] = value
}
