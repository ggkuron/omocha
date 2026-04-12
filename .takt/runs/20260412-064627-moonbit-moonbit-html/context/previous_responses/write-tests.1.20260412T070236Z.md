## 作業完了

### 作成したテストファイル

| ファイル | 件数 | 目的 |
|---------|------|------|
| `test/browser/check_files.sh` | 10件 | ブラウザ起動に必要なファイルの存在確認 |
| `test/browser/check_html.sh` | 17件 | `index.html` の内容検証 |
| `test/browser/playwright_smoke.sh` | — | Playwright によるスクリーンショット確認スクリプト |

### 現状の実行結果

- `check_files.sh`: PASS=0 FAIL=10（実装前のため全件 FAIL。**想定内**）
- `check_html.sh`: PASS=0 FAIL=1（`index.html` 未作成のため FAIL。**想定内**）
- スクリプト自体の構文エラーなし

### check_html.sh が検証する主な観点

- `<canvas id="app" width="800" height="600">` の存在（`id="app"` は kagura エンジンの固定要件）
- `<script type="module">` の使用（ES module import に必須）
- `kagura-init.js` / `kagura-gfx.js` / `kagura-audio.js` / `main.js` への参照
- WebGPU 初期化の正しい順序（`setupGlobalState` → `installAudioHelpers` → `installGfxHelpers` → `loadGameScript`）
- シャドウバース風 UI 要素（ダークテーマ CSS、操作説明）