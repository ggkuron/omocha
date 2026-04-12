# mizchi/glfw - GLFW MoonBit Bindings

## 最優先タスク: Windows 対応

現在 macOS のみ対応。Windows 対応を最優先で進める。

### macOS 依存箇所（要抽象化）

1. **タッチ入力** (`moonbit_update_touches`) — NSTouch/NSWindow/Cocoa API
2. **ウィンドウ作成時のタッチ設定** (`moonbit_glfw_create_window_safe` 末尾) — glfwGetCocoaWindow

### クロスプラットフォーム（変更不要）

ウィンドウ管理、キーボード、マウス、スクロール、ゲームパッド — 純粋な GLFW C API

### 対応方針

- `glfw_stub.m` → `glfw_stub.c` に変更し、`#ifdef __APPLE__` / `#ifdef _WIN32` でプラットフォーム分岐
- macOS 固有コード（タッチ入力）は `#ifdef __APPLE__` で囲む
- Windows ではタッチ入力を no-op（将来的に Win32 Touch API で実装可能）
- `moon.pkg` の framework リンクも `#ifdef` 相当の条件分岐が必要

## プロジェクト構成

- `src/glfw.mbt` - MoonBit FFI 宣言 + pub ラッパー関数
- `src/glfw_stub.m` - Objective-C ネイティブスタブ（macOS Cocoa → クロスプラットフォーム化予定）
- `src/moon.pkg` - パッケージ設定（native-only）

## ビルド・確認

```bash
just check    # moon check --target native
just fmt      # moon fmt + diff check
```

## 注意事項

- native ターゲット専用。JS/WASM ビルドは非対応
- `extern "C"` を含む `.mbt` は `moon.pkg` の `targets` で native に制限すること（`supported-targets` だけでは不十分）
- consumer 側で `cc-link-flags` に `-lglfw` が必要（link flags は依存から伝播しない）
- MoonBit は依存パッケージの `cc`/`cc-flags` をコンシューマーに伝播しない（CI では `CPATH` 等で回避）

## kagura からの開発

kagura 側で `moon.mod.json` をローカルパスに切り替えて開発:

```json
"mizchi/glfw": { "path": "../glfw-mbt" }
```

修正後は `moon publish` してバージョン番号に戻す。
