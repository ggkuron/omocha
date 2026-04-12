///|
/// Get the frame count of an audio source.
pub fn source_frame_count(source : AudioSource) -> Int {
  match source {
    Buffer(buf) => frame_count(buf)
    Stream(s) => s.ring.frames_available
  }
}

///|
/// Get the sample rate of an audio source.
pub fn source_sample_rate(source : AudioSource) -> Int {
  match source {
    Buffer(buf) => buf.sample_rate
    Stream(s) => s.sample_rate
  }
}

///|
/// Get the channel count of an audio source.
pub fn source_channels(source : AudioSource) -> Int {
  match source {
    Buffer(buf) => buf.channels
    Stream(s) => s.channels
  }
}

///|
/// Read a sample from an audio source at the given frame and channel.
pub fn read_source_sample(
  source : AudioSource,
  frame : Int,
  channel : Int,
) -> Float {
  match source {
    Buffer(buf) => {
      let fc = frame_count(buf)
      if frame < 0 || frame >= fc {
        return 0.0
      }
      get_sample(buf, frame, channel)
    }
    Stream(s) => ring_peek(s.ring, frame, channel)
  }
}
