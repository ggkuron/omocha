# TODO (audio-mbt)

## P0: Playback baseline ✅

- [x] PCM frame contract (`f32`, interleaved)
- [x] Mixer (multiple voices, gain, pan)
- [x] WAV decoder (PCM)
- [x] Output backend hooks (WebAudio/CoreAudio)
- [x] Deterministic tick integration with game runtime

## P1: Streaming and timing ✅

- [x] Streaming buffer model (pull/push)
- [x] Resampler (quality presets)
- [x] Latency/drift monitoring
- [x] Voice lifecycle (start/stop/pause/seek)
- [x] Concurrency-safe command queue

## P2: Formats and effects ✅

- [x] OGG/Vorbis decoder integration
- [x] Loop points and envelope helpers
- [x] Basic effects chain (lowpass/highpass/delay)
- [x] Asset cache policy for long tracks vs SFX

## Packaging ✅

- [x] Split reusable DSP core from backend adapters
- [x] Stabilize public API and add compatibility tests
