///|
/// Create a new streaming audio source.
pub fn new_streaming_source(
  pull : PullCallback,
  channels : Int,
  sample_rate : Int,
  buffer_frames? : Int = 4096,
) -> StreamingSource {
  StreamingSource::{
    ring: new_ring_buffer(buffer_frames, channels),
    pull,
    channels,
    sample_rate,
    ended: false,
    total_pulled: 0,
  }
}

///|
/// Fill the streaming source's ring buffer until at least min_frames are available.
pub fn streaming_fill(source : StreamingSource, min_frames : Int) -> Unit {
  if source.ended {
    return
  }
  let temp_size = 1024
  let temp : FixedArray[Float] = FixedArray::make(
    temp_size * source.channels,
    0.0,
  )
  while source.ring.frames_available < min_frames && not(source.ended) {
    let free = ring_free(source.ring)
    if free == 0 {
      break
    }
    let request = if free < temp_size { free } else { temp_size }
    let pulled = (source.pull.0)(temp, request)
    if pulled <= 0 {
      source.ended = true
      break
    }
    let _ = ring_write(source.ring, temp, pulled)
    source.total_pulled = source.total_pulled + pulled
  }
}

///|
/// Number of frames available in the streaming source.
pub fn streaming_available(source : StreamingSource) -> Int {
  ring_available(source.ring)
}
