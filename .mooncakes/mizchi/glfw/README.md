# mizchi/glfw

GLFW bindings for MoonBit (native target only).

Provides window management, keyboard/mouse/touch/gamepad input, fullscreen control, and cursor handling via C FFI.

## Install

```
moon add mizchi/glfw
```

Requires GLFW installed on the system:

```bash
# macOS
brew install glfw

# Windows (vcpkg)
vcpkg install glfw3

# Linux (apt)
sudo apt install libglfw3-dev
```

Consumer projects need `-lglfw` in their `cc-link-flags` (link flags do not propagate through dependencies).

## Usage

```moonbit
fn main {
  if @glfw.glfw_init() {
    let window = @glfw.create_window(800, 600, "Hello")
    while not(@glfw.window_should_close(window)) {
      @glfw.poll_events()
      // render...
    }
    @glfw.destroy_window(window)
    @glfw.terminate()
  }
}
```

## API

### Window

| Function | Description |
|----------|-------------|
| `glfw_init() -> Bool` | Initialize GLFW |
| `terminate()` | Terminate GLFW |
| `create_window(width, height, title) -> GLFWwindow` | Create window (NO_API hint for WebGPU) |
| `destroy_window(window)` | Destroy window |
| `window_should_close(window) -> Bool` | Check close flag |
| `set_window_should_close(window, value)` | Set close flag |
| `get_window_size(window) -> WindowSize` | Get window size |
| `get_window_width(window) -> Int` | Get window width |
| `get_window_height(window) -> Int` | Get window height |
| `get_framebuffer_size(window) -> WindowSize` | Get framebuffer size (Retina) |
| `get_window_content_scale(window) -> Double` | Get DPI scale factor |
| `set_fullscreen(window, enabled) -> Bool` | Toggle fullscreen |
| `is_fullscreen(window) -> Bool` | Check fullscreen state |
| `request_window_attention(window)` | Flash taskbar/dock |
| `swap_buffers(window)` | Swap front/back buffers |
| `set_swap_interval(enabled)` | Enable/disable vsync |
| `poll_events()` | Poll input events |

### Input

| Function | Description |
|----------|-------------|
| `get_cursor_x(window) -> Double` | Cursor X position |
| `get_cursor_y(window) -> Double` | Cursor Y position |
| `set_cursor_mode(window, mode) -> Int` | Set cursor mode (0=normal, 1=hidden, 2=disabled) |
| `get_cursor_mode(window) -> Int` | Get cursor mode |
| `take_scroll_x(window) -> Double` | Consume scroll X delta |
| `take_scroll_y(window) -> Double` | Consume scroll Y delta |
| `pressed_key_count(window) -> Int` | Number of pressed keys |
| `pressed_key_at(window, index) -> Int` | GLFW key code at index |
| `pressed_mouse_button_count(window) -> Int` | Number of pressed mouse buttons |
| `pressed_mouse_button_at(window, index) -> Int` | Mouse button at index |
| `touch_count(window) -> Int` | Number of active touches (macOS trackpad) |
| `touch_id_at(window, index) -> Int` | Touch ID at index |
| `touch_x_at(window, index) -> Double` | Touch X at index |
| `touch_y_at(window, index) -> Double` | Touch Y at index |
| `touch_type_at(window, index) -> Int` | Touch type (0=direct, 1=indirect) |
| `gamepad_count() -> Int` | Number of connected gamepads |
| `gamepad_id_at(index) -> Int` | Gamepad joystick ID |
| `gamepad_axis_count(index) -> Int` | Number of axes |
| `gamepad_axis_at(gamepad, axis) -> Double` | Axis value |
| `gamepad_pressed_button_count(index) -> Int` | Number of pressed buttons |
| `gamepad_pressed_button_at(gamepad, button) -> Int` | Button at index |

## Platform Support

| Platform | Status |
|----------|--------|
| macOS (ARM/x86) | Supported (touch input via Cocoa) |
| Windows | Builds on CI, not yet tested at runtime (touch input: no-op) |
| Linux | Builds on CI, not yet tested at runtime (touch input: no-op) |

## License

Apache-2.0
