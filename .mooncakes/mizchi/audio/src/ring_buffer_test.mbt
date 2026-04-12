///|
test "ring_buffer write and peek" {
  let rb = @audio.new_ring_buffer(4, 1)
  inspect(@audio.ring_available(rb), content="0")
  inspect(@audio.ring_free(rb), content="4")
  let src : FixedArray[Float] = [0.1, 0.2, 0.3]
  let written = @audio.ring_write(rb, src, 3)
  inspect(written, content="3")
  inspect(@audio.ring_available(rb), content="3")
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 0, 0), 0.1, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 1, 0), 0.2, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 2, 0), 0.3, absolute_tolerance=tol),
  )
}

///|
test "ring_buffer consume" {
  let rb = @audio.new_ring_buffer(4, 1)
  let src : FixedArray[Float] = [1.0, 2.0, 3.0, 4.0]
  let _ = @audio.ring_write(rb, src, 4)
  @audio.ring_consume(rb, 2)
  inspect(@audio.ring_available(rb), content="2")
  let tol : Float = 0.0001
  // After consuming 2, peek(0) should be the 3rd element
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 0, 0), 3.0, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 1, 0), 4.0, absolute_tolerance=tol),
  )
}

///|
test "ring_buffer wrap-around" {
  let rb = @audio.new_ring_buffer(4, 1)
  let src1 : FixedArray[Float] = [1.0, 2.0, 3.0]
  let _ = @audio.ring_write(rb, src1, 3)
  @audio.ring_consume(rb, 2)
  // write_pos=3, read_pos=2, available=1
  let src2 : FixedArray[Float] = [4.0, 5.0]
  let written = @audio.ring_write(rb, src2, 2)
  inspect(written, content="2")
  inspect(@audio.ring_available(rb), content="3")
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 0, 0), 3.0, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 1, 0), 4.0, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 2, 0), 5.0, absolute_tolerance=tol),
  )
}

///|
test "ring_buffer overflow clamped" {
  let rb = @audio.new_ring_buffer(2, 1)
  let src : FixedArray[Float] = [1.0, 2.0, 3.0]
  let written = @audio.ring_write(rb, src, 3)
  // Only 2 frames should be written (capacity=2)
  inspect(written, content="2")
  inspect(@audio.ring_available(rb), content="2")
}

///|
test "ring_buffer stereo" {
  let rb = @audio.new_ring_buffer(4, 2)
  // 2 frames of stereo data: [L0, R0, L1, R1]
  let src : FixedArray[Float] = [0.1, 0.2, 0.3, 0.4]
  let written = @audio.ring_write(rb, src, 2)
  inspect(written, content="2")
  let tol : Float = 0.0001
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 0, 0), 0.1, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 0, 1), 0.2, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 1, 0), 0.3, absolute_tolerance=tol),
  )
  assert_true(
    Float::is_close(@audio.ring_peek(rb, 1, 1), 0.4, absolute_tolerance=tol),
  )
}

///|
test "ring_buffer reset" {
  let rb = @audio.new_ring_buffer(4, 1)
  let src : FixedArray[Float] = [1.0, 2.0]
  let _ = @audio.ring_write(rb, src, 2)
  @audio.ring_reset(rb)
  inspect(@audio.ring_available(rb), content="0")
  inspect(@audio.ring_free(rb), content="4")
}

///|
test "ring_peek out of bounds returns 0" {
  let rb = @audio.new_ring_buffer(4, 1)
  let src : FixedArray[Float] = [1.0]
  let _ = @audio.ring_write(rb, src, 1)
  assert_true(@audio.ring_peek(rb, -1, 0) == (0.0 : Float))
  assert_true(@audio.ring_peek(rb, 5, 0) == (0.0 : Float))
}
