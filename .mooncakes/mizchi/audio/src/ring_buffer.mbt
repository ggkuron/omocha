///|
/// Create a new ring buffer with the given capacity in frames.
pub fn new_ring_buffer(capacity : Int, channels : Int) -> RingBuffer {
  RingBuffer::{
    data: FixedArray::make(capacity * channels, 0.0),
    capacity,
    channels,
    write_pos: 0,
    read_pos: 0,
    frames_available: 0,
  }
}

///|
/// Number of frames available for reading.
pub fn ring_available(rb : RingBuffer) -> Int {
  rb.frames_available
}

///|
/// Number of frames of free space for writing.
pub fn ring_free(rb : RingBuffer) -> Int {
  rb.capacity - rb.frames_available
}

///|
/// Write frames into the ring buffer. Returns number of frames actually written.
pub fn ring_write(
  rb : RingBuffer,
  src : FixedArray[Float],
  frames : Int,
) -> Int {
  let to_write = if frames > ring_free(rb) { ring_free(rb) } else { frames }
  for f in 0..<to_write {
    for ch in 0..<rb.channels {
      let src_idx = f * rb.channels + ch
      let dst_idx = rb.write_pos * rb.channels + ch
      rb.data[dst_idx] = src[src_idx]
    }
    rb.write_pos = (rb.write_pos + 1) % rb.capacity
  }
  rb.frames_available = rb.frames_available + to_write
  to_write
}

///|
/// Peek at a sample at the given frame offset from read_pos.
pub fn ring_peek(rb : RingBuffer, frame_offset : Int, channel : Int) -> Float {
  if frame_offset < 0 || frame_offset >= rb.frames_available {
    return 0.0
  }
  let idx = (rb.read_pos + frame_offset) % rb.capacity * rb.channels + channel
  rb.data[idx]
}

///|
/// Consume (advance read_pos) the given number of frames.
pub fn ring_consume(rb : RingBuffer, frames : Int) -> Unit {
  let to_consume = if frames > rb.frames_available {
    rb.frames_available
  } else {
    frames
  }
  rb.read_pos = (rb.read_pos + to_consume) % rb.capacity
  rb.frames_available = rb.frames_available - to_consume
}

///|
/// Reset the ring buffer to empty state.
pub fn ring_reset(rb : RingBuffer) -> Unit {
  rb.write_pos = 0
  rb.read_pos = 0
  rb.frames_available = 0
}
