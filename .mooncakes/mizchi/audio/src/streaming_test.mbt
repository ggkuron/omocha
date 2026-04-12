///|
test "streaming_fill from callback" {
  // Callback generates incrementing samples
  let counter = Ref::new(0)
  let pull = @audio.PullCallback(fn(
    buf : FixedArray[Float],
    frames : Int,
  ) -> Int {
    for i in 0..<frames {
      buf[i] = Float::from_int(counter.val + i) * 0.1
    }
    counter.val = counter.val + frames
    frames
  })
  let src = @audio.new_streaming_source(pull, 1, 44100, buffer_frames=16)
  inspect(@audio.streaming_available(src), content="0")
  @audio.streaming_fill(src, 4)
  assert_true(@audio.streaming_available(src) >= 4)
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(
      @audio.ring_peek(src.ring, 0, 0),
      0.0,
      absolute_tolerance=tol,
    ),
  )
  assert_true(
    Float::is_close(
      @audio.ring_peek(src.ring, 1, 0),
      0.1,
      absolute_tolerance=tol,
    ),
  )
}

///|
test "streaming_fill ended flag" {
  let calls = Ref::new(0)
  let pull = @audio.PullCallback(fn(
    _buf : FixedArray[Float],
    _frames : Int,
  ) -> Int {
    calls.val = calls.val + 1
    if calls.val <= 1 {
      // First call: provide 2 frames
      2
    } else {
      // Second call: signal end
      0
    }
  })
  let src = @audio.new_streaming_source(pull, 1, 44100, buffer_frames=16)
  @audio.streaming_fill(src, 10)
  assert_true(src.ended)
  inspect(src.total_pulled, content="2")
}

///|
test "streaming source in mixer" {
  // Create a streaming source that provides known samples
  let pull = @audio.PullCallback(fn(
    buf : FixedArray[Float],
    frames : Int,
  ) -> Int {
    for i in 0..<frames {
      buf[i] = 1.0
    }
    frames
  })
  let stream = @audio.new_streaming_source(pull, 1, 44100, buffer_frames=256)
  // Pre-fill the ring buffer
  @audio.streaming_fill(stream, 4)
  let src = @audio.AudioSource::Stream(stream)
  // Verify we can read samples from the stream source
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(
      @audio.read_source_sample(src, 0, 0),
      1.0,
      absolute_tolerance=tol,
    ),
  )
  assert_true(
    Float::is_close(
      @audio.read_source_sample(src, 1, 0),
      1.0,
      absolute_tolerance=tol,
    ),
  )
}
