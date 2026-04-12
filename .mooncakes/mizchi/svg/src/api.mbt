///|
/// Public API helpers
/// Convenience functions for rendering to Image and bridging external trees.

///|
/// Render an SVGDocument into an Image.
pub fn render_svg_document_to_image(
  doc : SVGDocument,
  width : Int,
  height : Int,
) -> Image {
  let image = Image::new(width, height)
  let setter : PixelSetter = {
    set: (x, y, color) => image.set_pixel(x, y, color),
  }
  let ctx = RenderContext::new(setter, width, height)
  doc.render(ctx)
  image
}

///|
/// Render a Scene into an Image.
pub fn render_svg_scene_to_image(
  scene : Scene,
  width : Int,
  height : Int,
) -> Image {
  let image = Image::new(width, height)
  let setter : PixelSetter = {
    set: (x, y, color) => image.set_pixel(x, y, color),
  }
  let ctx = RenderContext::new(setter, width, height)
  scene.render(ctx)
  image
}

///|
/// Render a raw SVGNode tree into an Image.
pub fn render_svg_node_to_image(
  node : SVGNode,
  width : Int,
  height : Int,
) -> Image {
  let scene = Scene::new(node)
  render_svg_scene_to_image(scene, width, height)
}

///|
/// Parse and render an SVG markup string into an Image.
pub fn render_svg_to_image(
  svg_str : String,
  width : Int,
  height : Int,
) -> Image? {
  parse_svg_document(svg_str).map(doc => render_svg_document_to_image(
    doc, width, height,
  ))
}
