///|
pub(all) enum ResizeMethod {
  Nearest
  Bilinear
  Bicubic
} derive(Eq, Show)

///|
pub(all) struct ImageData {
  width : Int
  height : Int
  data : Bytes
} derive(Eq, Show)

///|
pub enum ColorType {
  Grayscale
  Rgb
  Indexed
  GrayscaleAlpha
  Rgba
} derive(Eq, Show)

///|
pub(all) struct IhdrData {
  width : Int
  height : Int
  bit_depth : Int
  color_type : ColorType
  compression : Int
  filter : Int
  interlace : Int
} derive(Eq, Show)

///|
pub(all) enum ImageFormat {
  Png
  Bmp
} derive(Eq, Show)

///|
pub(all) struct StreamImageInfo {
  format : ImageFormat
  width : Int
  height : Int
} derive(Eq, Show)

///|
pub(all) suberror DecodeError {
  InvalidSignature(String)
  InvalidChunkCrc(String)
  MissingChunk(String)
  UnsupportedFeature(String)
  CorruptData(String)
}

///|
pub impl Show for DecodeError with output(self, logger) {
  match self {
    InvalidSignature(s) => logger.write_string("InvalidSignature: " + s)
    InvalidChunkCrc(s) => logger.write_string("InvalidChunkCrc: " + s)
    MissingChunk(s) => logger.write_string("MissingChunk: " + s)
    UnsupportedFeature(s) => logger.write_string("UnsupportedFeature: " + s)
    CorruptData(s) => logger.write_string("CorruptData: " + s)
  }
}

///|
pub(all) suberror EncodeError {
  InvalidDimensions(String)
  InvalidData(String)
}

///|
pub impl Show for EncodeError with output(self, logger) {
  match self {
    InvalidDimensions(s) => logger.write_string("InvalidDimensions: " + s)
    InvalidData(s) => logger.write_string("InvalidData: " + s)
  }
}

///|
fn color_type_from_png(value : Int) -> ColorType raise DecodeError {
  match value {
    0 => Grayscale
    2 => Rgb
    3 => Indexed
    4 => GrayscaleAlpha
    6 => Rgba
    _ => raise UnsupportedFeature("unknown color type: " + value.to_string())
  }
}

///|
fn color_type_to_png(ct : ColorType) -> Int {
  match ct {
    Grayscale => 0
    Rgb => 2
    Indexed => 3
    GrayscaleAlpha => 4
    Rgba => 6
  }
}

///|
fn bytes_per_pixel(ct : ColorType, bit_depth : Int) -> Int {
  let channels = match ct {
    Grayscale => 1
    Rgb => 3
    Indexed => 1
    GrayscaleAlpha => 2
    Rgba => 4
  }
  let bits = channels * bit_depth
  if bits < 8 {
    1
  } else {
    bits / 8
  }
}

///|
test "color_type_from_png" {
  assert_eq(color_type_from_png(0), Grayscale)
  assert_eq(color_type_from_png(2), Rgb)
  assert_eq(color_type_from_png(3), Indexed)
  assert_eq(color_type_from_png(4), GrayscaleAlpha)
  assert_eq(color_type_from_png(6), Rgba)
}

///|
test "color_type_to_png" {
  assert_eq(color_type_to_png(Grayscale), 0)
  assert_eq(color_type_to_png(Rgb), 2)
  assert_eq(color_type_to_png(Indexed), 3)
  assert_eq(color_type_to_png(GrayscaleAlpha), 4)
  assert_eq(color_type_to_png(Rgba), 6)
}

///|
test "bytes_per_pixel" {
  assert_eq(bytes_per_pixel(Grayscale, 8), 1)
  assert_eq(bytes_per_pixel(Rgb, 8), 3)
  assert_eq(bytes_per_pixel(Rgba, 8), 4)
  assert_eq(bytes_per_pixel(GrayscaleAlpha, 8), 2)
  assert_eq(bytes_per_pixel(Indexed, 8), 1)
}
