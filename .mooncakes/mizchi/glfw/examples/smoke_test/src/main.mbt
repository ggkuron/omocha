fn main {
  println("=== GLFW Smoke Test ===")
  if @glfw.glfw_init() {
    println("OK: GLFW initialized")
    let window = @glfw.create_window(640, 480, "Smoke Test")
    let w = @glfw.get_window_width(window)
    let h = @glfw.get_window_height(window)
    println("OK: Window created (\{w}x\{h})")
    @glfw.poll_events()
    println("OK: Events polled")
    // Touch (no-op on Linux)
    let tc = @glfw.touch_count(window)
    println("OK: Touch count = \{tc}")
    // Gamepad
    let gc = @glfw.gamepad_count()
    println("OK: Gamepad count = \{gc}")
    @glfw.destroy_window(window)
    println("OK: Window destroyed")
    @glfw.terminate()
    println("=== All tests passed ===")
  } else {
    println("ERROR: GLFW initialization failed")
  }
}
