///|
/// Monitors audio latency and drift between producer and consumer.
pub(all) struct LatencyMonitor {
  mut frames_written : Int
  mut ticks_elapsed : Int
  mut drift_frames_val : Int
  mut underrun_count : Int
  mut max_drift : Int
}

///|
/// Create a new latency monitor.
pub fn new_latency_monitor() -> LatencyMonitor {
  LatencyMonitor::{
    frames_written: 0,
    ticks_elapsed: 0,
    drift_frames_val: 0,
    underrun_count: 0,
    max_drift: 0,
  }
}

///|
/// Update latency tracking after a tick.
/// frames_this_tick: frames produced this tick.
/// consumed: frames consumed by backend since last update.
pub fn update_latency(
  monitor : LatencyMonitor,
  frames_this_tick : Int,
  consumed : Int,
) -> Unit {
  monitor.frames_written = monitor.frames_written + frames_this_tick
  monitor.ticks_elapsed = monitor.ticks_elapsed + 1
  monitor.drift_frames_val = monitor.drift_frames_val +
    frames_this_tick -
    consumed
  if consumed > frames_this_tick {
    monitor.underrun_count = monitor.underrun_count + 1
  }
  let abs_drift = if monitor.drift_frames_val >= 0 {
    monitor.drift_frames_val
  } else {
    -monitor.drift_frames_val
  }
  if abs_drift > monitor.max_drift {
    monitor.max_drift = abs_drift
  }
}

///|
/// Get current drift in frames.
pub fn drift_frames(monitor : LatencyMonitor) -> Int {
  monitor.drift_frames_val
}

///|
/// Get current drift in seconds.
pub fn drift_secs(monitor : LatencyMonitor, sample_rate : Int) -> Float {
  Float::from_int(monitor.drift_frames_val) / Float::from_int(sample_rate)
}

///|
/// Get underrun count.
pub fn underrun_count(monitor : LatencyMonitor) -> Int {
  monitor.underrun_count
}

///|
/// Reset all counters.
pub fn reset_latency_monitor(monitor : LatencyMonitor) -> Unit {
  monitor.frames_written = 0
  monitor.ticks_elapsed = 0
  monitor.drift_frames_val = 0
  monitor.underrun_count = 0
  monitor.max_drift = 0
}
