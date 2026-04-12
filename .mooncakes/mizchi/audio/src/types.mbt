///|
/// PCM audio buffer with interleaved sample data.
pub(all) struct AudioBuffer {
  channels : Int
  sample_rate : Int
  data : FixedArray[Float]
} derive(Eq, Show)

///|
/// Stable voice identifier.
pub(all) struct VoiceId(Int) derive(Eq, Show, Compare, Hash)

///|
/// Ring buffer for streaming audio data.
pub(all) struct RingBuffer {
  data : FixedArray[Float]
  capacity : Int
  channels : Int
  mut write_pos : Int
  mut read_pos : Int
  mut frames_available : Int
} derive(Show)

///|
/// Callback that pulls audio data into a buffer. Returns frames actually written.
pub(all) struct PullCallback((FixedArray[Float], Int) -> Int)

///|
/// Streaming audio source backed by a ring buffer.
pub(all) struct StreamingSource {
  ring : RingBuffer
  pull : PullCallback
  channels : Int
  sample_rate : Int
  mut ended : Bool
  mut total_pulled : Int
}

///|
pub impl Show for PullCallback with output(self, logger) {
  ignore(self)
  logger.write_string("<PullCallback>")
}

///|
pub impl Show for StreamingSource with output(self, logger) {
  logger.write_string(
    "StreamingSource { channels: \{self.channels}, sample_rate: \{self.sample_rate}, ended: \{self.ended} }",
  )
}

///|
/// Audio source: either a complete buffer or a streaming source.
pub(all) enum AudioSource {
  Buffer(AudioBuffer)
  Stream(StreamingSource)
} derive(Show)

///|
/// Voice playback state.
pub(all) enum VoiceState {
  Playing
  Paused
  Stopped
} derive(Eq, Show)

///|
/// A single voice playing an audio source.
pub(all) struct Voice {
  id : VoiceId
  source : AudioSource
  mut position : Float
  mut gain : Float
  mut pan : Float
  mut state : VoiceState
  looping : Bool
  sample_rate : Int
  loop_start : Int // frame index to loop back to (default 0)
  loop_end : Int // frame index where loop restarts (0 = buffer end)
  envelope : Envelope?
  effects : Array[EffectNode]
} derive(Show)

///|
/// Resampling quality presets.
pub(all) enum ResampleQuality {
  Nearest
  Linear
  Cubic
} derive(Eq, Show)

///|
/// Multi-voice mixer with master gain.
pub(all) struct Mixer {
  mut voices : Array[Voice]
  mut master_gain : Float
  channels : Int
  sample_rate : Int
  resample_quality : ResampleQuality
  mut next_voice_id : Int
}

///|
/// ADSR envelope configuration.
pub(all) struct EnvelopeConfig {
  attack : Float // seconds
  decay : Float // seconds
  sustain : Float // level [0.0, 1.0]
  release_time : Float // seconds
} derive(Eq, Show)

///|
/// Envelope phase in ADSR state machine.
pub(all) enum EnvelopePhase {
  Attack
  Decay
  Sustain
  Release
  Done
} derive(Eq, Show)

///|
/// ADSR envelope state machine.
pub(all) struct Envelope {
  config : EnvelopeConfig
  sample_rate : Int
  mut phase : EnvelopePhase
  mut level : Float
  mut phase_position : Int
  mut release_start_level : Float
} derive(Show)

///|
/// Biquad filter state (Direct Form II Transposed).
pub(all) struct BiquadState {
  b0 : Float
  b1 : Float
  b2 : Float
  a1 : Float
  a2 : Float
  mut z1 : Float
  mut z2 : Float
} derive(Show)

///|
/// Delay effect state.
pub(all) struct DelayState {
  buffer : FixedArray[Float]
  mut write_pos : Int
  delay_samples : Int
  feedback : Float
  mix : Float
} derive(Show)

///|
/// Effect node in the processing chain.
pub(all) enum EffectNode {
  Lowpass(BiquadState)
  Highpass(BiquadState)
  Delay(DelayState)
} derive(Show)

///|
/// Cache eviction policy for audio assets.
pub(all) enum CachePolicy {
  AlwaysCache
  Normal
  StreamPrefer
} derive(Eq, Show)

///|
/// Single entry in the asset cache.
pub(all) struct CacheEntry {
  buffer : AudioBuffer
  policy : CachePolicy
  byte_size : Int
  mut last_access : Int
} derive(Show)

///|
/// LRU asset cache for audio buffers.
pub(all) struct AssetCache {
  entries : Map[String, CacheEntry]
  mut total_bytes : Int
  max_bytes : Int
  max_entries : Int
  mut access_counter : Int
} derive(Show)

///|
/// LSB-first bit reader for Vorbis bitstream parsing.
struct BitReader {
  data : Bytes
  length_bits : Int
  mut bit_pos : Int
} derive(Show)

///|
/// OGG page header.
struct OggPage {
  header_type : Int
  granule_position : Int64
  serial_number : Int
  page_sequence : Int
  segments : Array[Bytes]
} derive(Show)

///|
/// OGG container demuxer.
struct OggDemuxer {
  data : Bytes
  mut offset : Int
  mut current_packet : Array[Bytes]
} derive(Show)

///|
/// Audio subsystem errors.
pub suberror AudioError {
  InvalidFormat(String)
  DecodeFailed(String)
}

///|
pub impl Show for AudioError with output(self, logger) {
  match self {
    InvalidFormat(msg) => logger.write_string("InvalidFormat(\{msg})")
    DecodeFailed(msg) => logger.write_string("DecodeFailed(\{msg})")
  }
}

///|
test "AudioError display" {
  let err : AudioError = InvalidFormat("bad header")
  inspect(err, content="InvalidFormat(bad header)")
  let err2 : AudioError = DecodeFailed("truncated")
  inspect(err2, content="DecodeFailed(truncated)")
}
