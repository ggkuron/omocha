# TODO (image-mbt)

## Done

- [x] PNG decoder (IHDR/PLTE/IDAT/IEND, all color types)
- [x] PNG encoder (RGBA8, adaptive row filtering)
- [x] zlib/deflate integration (`mizchi/zlib`)
- [x] Color type conversion to RGBA8
- [x] Error model and diagnostics (`DecodeError` / `EncodeError`)
- [x] Engine-ready `ImageData` contract
- [x] Golden tests with fixture corpus
- [x] JPEG baseline decode
- [x] BMP decode / encode (24-bit uncompressed)
- [x] Image resize (Nearest / Bilinear / Bicubic)

## Next

- [x] JPEG encoder
- [ ] WebP support
- [ ] Zero-copy decode path for js/wasm where possible
- [ ] Streaming decode API for large assets
- [ ] Metadata handling policy (gamma/sRGB/ICC)

## Packaging

- [ ] Split reusable codec core from engine-specific adapters
- [ ] Stabilize public API and add compatibility tests
