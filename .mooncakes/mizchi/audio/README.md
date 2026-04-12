# mizchi/audio

Audio runtime primitives for MoonBit game workloads.

## Features

- **Mixer** -- multi-voice playback with per-voice gain, pan, and loop points
- **Decoders** -- WAV (PCM 16-bit) and OGG/Vorbis
- **Resampler** -- Nearest / Linear / Cubic interpolation
- **Effects** -- biquad lowpass/highpass, delay, chainable effect nodes
- **Envelope** -- ADSR envelope generator
- **Streaming** -- pull-based ring buffer streaming with backpressure
- **Runtime** -- deterministic tick-driven audio runtime with command queue
- **Backend hooks** -- platform-agnostic output (WebAudio, CoreAudio, etc.)
- **Asset cache** -- LRU cache with eviction policies for audio buffers

## Install

```
moon add mizchi/audio
```

## Quick Start

```moonbit
// Decode a WAV file
let buffer = @audio.decode_wav(wav_bytes)

// Create a mixer and play a sound
let mixer = @audio.new_mixer(44100)
let voice = @audio.new_voice_from_buffer(mixer, buffer)
let id = @audio.add_voice(mixer, voice)

// Render audio frames
let output = FixedArray::make(512, (0.0 : Float))
@audio.tick(mixer, 256, output)

// Or use the high-level runtime
let rt = @audio.new_audio_runtime(44100, 60) // sample_rate, tps
let _id = @audio.play_sound(rt, buffer)
@audio.audio_tick(rt)
```

## Targets

- `js` (primary)
- `native`

## Development

```bash
just               # check + test (js)
just release-check # fmt + info + check + test
just release-check-all # above + native target
```

## License

Apache-2.0
