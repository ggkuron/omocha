///|
test "latency monitor frames tracking" {
  let monitor = @audio.new_latency_monitor()
  // 10 ticks x 735 frames, consumed matches perfectly
  for _i in 0..<10 {
    @audio.update_latency(monitor, 735, 735)
  }
  inspect(monitor.frames_written, content="7350")
  inspect(monitor.ticks_elapsed, content="10")
  inspect(@audio.drift_frames(monitor), content="0")
}

///|
test "latency monitor drift accumulates" {
  let monitor = @audio.new_latency_monitor()
  // Produce 735, consume 700 -> drift +35 per tick
  for _i in 0..<5 {
    @audio.update_latency(monitor, 735, 700)
  }
  inspect(@audio.drift_frames(monitor), content="175")
  inspect(monitor.max_drift, content="175")
}

///|
test "drift_secs calculation" {
  let monitor = @audio.new_latency_monitor()
  @audio.update_latency(monitor, 4410, 0)
  let secs = @audio.drift_secs(monitor, 44100)
  let tol : Float = 0.0001
  assert_true(Float::is_close(secs, 0.1, absolute_tolerance=tol))
}

///|
test "underrun detection" {
  let monitor = @audio.new_latency_monitor()
  // Normal tick
  @audio.update_latency(monitor, 735, 735)
  inspect(@audio.underrun_count(monitor), content="0")
  // Underrun: consumed more than produced
  @audio.update_latency(monitor, 735, 800)
  inspect(@audio.underrun_count(monitor), content="1")
}

///|
test "reset latency monitor" {
  let monitor = @audio.new_latency_monitor()
  @audio.update_latency(monitor, 735, 700)
  @audio.update_latency(monitor, 735, 800)
  @audio.reset_latency_monitor(monitor)
  inspect(monitor.frames_written, content="0")
  inspect(monitor.ticks_elapsed, content="0")
  inspect(@audio.drift_frames(monitor), content="0")
  inspect(@audio.underrun_count(monitor), content="0")
  inspect(monitor.max_drift, content="0")
}
