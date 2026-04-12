以下が調査結果と計画のサマリーです。

---

## 調査結果

### `moonbit/` の状況

`moonbit/` ディレクトリは現ワークツリーには存在しておらず、元プロジェクト `/Users/yh/proj/astrata/` の作業ディレクトリに untracked ファイルとして存在しています。同一 git リポジトリの別ブランチなので、そこからコピーして使います。

### 既存の MoonBit 実装

| 項目 | 内容 |
|------|------|
| ゲームエンジン | `mizchi/kagura` (WebGPU ベース 2D エンジン) |
| エントリポイント | `moonbit/main/main.mbt` → canvas `#app`、800×600 |
| ビルド出力 | `_build/js/debug/build/main/main.js`（4479行・単一ファイルバンドル） |
| ビルドコマンド | `/Users/yh/.moon/bin/moon build --target js` |
| 依存 JS ライブラリ | `.mooncakes/mizchi/kagura/lib/web/` の 3 ファイル（kagura-init.js / gfx / audio） |

---

## 実装計画

### 追加ファイル（ワークツリーに新規作成・コピー）

```
moon.mod.json            ← /Users/yh/proj/astrata/ からコピー
moonbit/                 ← /Users/yh/proj/astrata/moonbit/ からコピー
.mooncakes/              ← /Users/yh/proj/astrata/.mooncakes/ からコピー (16MB)
lib/web/
  kagura-init.js         ← .mooncakes から抽出コピー
  kagura-gfx.js
  kagura-audio.js
_build/                  ← moon build --target js で生成
index.html               ← 新規作成（シャドウバース風 UI）
test/browser/
  check_files.sh         ← ファイル存在確認テスト
  check_html.sh          ← HTML 内容検証テスト
  playwright_smoke.sh    ← Playwright 確認スクリプト
```

### HTML の核心

- `<canvas id="app" width="800" height="600">` （id は`#app`固定、変更不可）
- `<script type="module">` で kagura libs をロード → WebGPU 初期化 → `loadGameScript("_build/.../main.js")`
- シャドウバース風ダークテーマ、gold glow ボーダー、操作説明パネル付き

### 起動手順

```bash
python3 -m http.server 8080  # 静的サーバー必須（file:// は ES module CORS で動作不可）
# → http://localhost:8080/
```

### 注意点（implement エージェント向け）

1. canvas `id="app"` は変更禁止（ゲームコード固定）
2. `<script type="module">` 必須
3. kagura 初期化順序: `setupGlobalState` → `installAudioHelpers` → `installGfxHelpers` → `loadGameScript`
4. WebGPU 必須（Chrome 113+ / Edge 113+）、エラー時は `showStartupError` で表示