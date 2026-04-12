# タスク計画

## 元の要求

`moonbit/` 配下にある生成済み MoonBit 実装をブラウザで起動できるようにし、ブラウザ起動用の HTML を用意する。そのうえで `playwright-cli` を使って実画面を確認しながら、UI をシャドウバース風のカードバトル UI に寄せて洗練させる。

---

## 分析結果

### 目的

MoonBit 実装（カードバトルゲーム "Astrata"）をブラウザで動作させる HTML/CSS/JS を整備し、シャドウバース風 UI に調整する。

---

### 分解した要件

| # | 要件 | 種別 | 備考 |
|---|------|------|------|
| 1 | `moonbit/` ソースをワークツリーに配置する | 明示 | 元プロジェクト `/Users/yh/proj/astrata/moonbit/` からコピー |
| 2 | `moon.mod.json` をワークツリーに配置する | 暗黙 | #1 の moonbit/ ビルドに必要 |
| 3 | `.mooncakes/` 依存をワークツリーに配置する | 暗黙 | #1 の moonbit/ ビルドに必要 |
| 4 | `moon build --target js` でビルドする | 明示 | 出力: `_build/js/debug/build/main/main.js` |
| 5 | `index.html` を作成する | 明示 | ブラウザ起動エントリポイント |
| 6 | `index.html` に `<canvas id="app" width="800" height="600">` を含める | 暗黙 | kagura エンジンが `#app` を固定セレクタとして要求（`main.mbt:9`） |
| 7 | kagura-init.js / kagura-gfx.js / kagura-audio.js を配置する | 暗黙 | `index.html` から ES module として読み込むため分離が必要 |
| 8 | `index.html` から WebGPU 初期化シーケンスを正しい順序で実行する | 明示 | setupGlobalState → installAudioHelpers → installGfxHelpers → loadGameScript |
| 9 | `python3 -m http.server 8080` で静的サーバーを起動し URL を明確にする | 明示 | `file://` では ES module CORS 制限により動作不可 |
| 10 | `playwright-cli` で `http://localhost:8080/` を開いて確認する | 明示 | スクリーンショット取得を含む |
| 11 | UI をシャドウバース風に調整する | 明示 | ダークテーマ・金色装飾・操作説明パネルを追加 |
| 12 | ファイル存在確認テストを作成する | 暗黙 | Playwright 基盤が未存在のためシェルスクリプトで代替 |
| 13 | HTML 内容検証テストを作成する | 暗黙 | canvas, kagura 参照, main.js 参照の確認 |

---

### 参照資料の調査結果

#### `moonbit/` 構成（`/Users/yh/proj/astrata/moonbit/`）

```
moonbit/
  lib/   ← ゲームロジック (cards, field, field_play, logic, types, *_wbtest)
  main/  ← エントリ・ビュー (main, game, view, game_*_wbtest)
```

**エントリポイント（`main/main.mbt`）:**
```moonbit
fn main {
  let game = Game::new()
  @scene.run_game(game, width=screen_w, height=screen_h, title="Astrata", canvas="#app")
}
```
- canvas セレクタ `"#app"` は固定（変更不可）
- 画面サイズ: 800×600（`view.mbt:1-2`）

**UI 実装（`main/view.mbt`）:**
- 上部（RIVAL エリア）: `#0f3460`
- 中央 HUD（ターン表示）: `#0a0a1a`
- 下部（OWN エリア）: `#16213e`
- オリジンカード: `#f0a500`（金色）
- サテライトスロット: `#4ecdc4`
- ハイライト: `#ffff00`

**操作キー（`main/game.mbt:20-46`）:**
- Left/Right 矢印: 選択横移動
- Up/Down 矢印: レベル内移動
- Enter（confirm）: 自分カードをプレイ
- E: ターン終了

#### 生成済みビルド成果物

- パス: `/Users/yh/proj/astrata/_build/js/debug/build/main/main.js`（4479行）
- 状態: 最新ビルド済み（`moon build` 実行で "no work to do"）
- 末尾の IIFE が `run_game` を呼び出してゲームループを起動する

#### Kagura エンジン（`.mooncakes/mizchi/kagura/lib/web/`）

- WebGPU ベース（Canvas 2D ではない）→ Chrome 113+ / Edge 113+ 必須
- 初期化シーケンス（`scripts/web-demo-pages.mjs:537-576` より確認）:
  1. `initWebGPU("#app")` → WebGPU デバイス取得
  2. `setupGlobalState(canvas, device, format, context)` → `globalThis.__kaguraWebRuntime` 設定
  3. `installAudioHelpers()` / `installGfxHelpers()` → FFI バインド
  4. `loadGameScript(scriptPath)` → main.js IIFE 実行
- `showStartupError("#app", message, detail)` でエラー表示（WebGPU 非対応ブラウザ向け）

#### ワークツリーの現状

- ブランチ: `takt/20260412T0647-tasuku-moonbit-niaru-mi-moonbi`
- 現在のファイル: Haskell プロジェクト（astrata.cabal, src/, test/ など）
- `moonbit/`, `moon.mod.json`, `.mooncakes/` は **存在しない**（元プロジェクトにのみ untracked で存在）

---

### スコープ

**対象ファイル（新規追加のみ、既存 Haskell コードへの影響ゼロ）:**
- `moon.mod.json`
- `moonbit/` （ソース全体）
- `.mooncakes/` （依存パッケージ 16MB）
- `lib/web/kagura-init.js`, `kagura-gfx.js`, `kagura-audio.js`
- `_build/` （ビルド生成物）
- `index.html`
- `test/browser/check_files.sh`
- `test/browser/check_html.sh`
- `test/browser/playwright_smoke.sh`

---

### 検討したアプローチ

| アプローチ | 採否 | 理由 |
|-----------|------|------|
| A: 元プロジェクトのビルド出力をシンボリックリンクで参照 | 不採用 | ブラウザは絶対パスを参照できない。静的サーバーはワークツリー内を serve する |
| B: `moon install` でワークツリーに依存を再取得 | 不採用 | ネットワーク依存・時間コストがかかる。元プロジェクトに `.mooncakes/` が既存のため cp で十分 |
| C: 元プロジェクトから moonbit/ + .mooncakes/ をコピーしてワークツリーでビルド | **採用** | 確実・高速・オフライン。既存ビルドが動作確認済みのため最もリスクが低い |
| D: Vite + vite-plugin-moonbit を使ったホットリロード開発環境 | 不採用 | タスクスコープ外（単純な静的サーバーで要件を満たす） |

---

### 実装アプローチ

**Step 1: ソースと依存のコピー**
```bash
cp /Users/yh/proj/astrata/moon.mod.json .
cp -r /Users/yh/proj/astrata/moonbit .
cp -r /Users/yh/proj/astrata/.mooncakes .
mkdir -p lib/web
cp .mooncakes/mizchi/kagura/lib/web/kagura-init.js lib/web/
cp .mooncakes/mizchi/kagura/lib/web/kagura-gfx.js lib/web/
cp .mooncakes/mizchi/kagura/lib/web/kagura-audio.js lib/web/
```

**Step 2: ビルド**
```bash
/Users/yh/.moon/bin/moon build --target js
# → _build/js/debug/build/main/main.js が生成される
```

**Step 3: index.html 作成**

構造:
```html
<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8">
  <title>Astrata – Card Battle</title>
  <style>
    /* シャドウバース風ダークテーマ */
    body { background: linear-gradient(#0d1117, #1a1a2e); }
    /* canvas 周囲に gold glow ボーダー */
    #app { border: 2px solid rgba(240,165,0,0.4);
           box-shadow: 0 0 24px rgba(240,165,0,0.2); }
    /* 操作説明パネル */
    .controls { color: #9fb0c7; font: 0.8rem monospace; }
  </style>
</head>
<body>
  <header><!-- タイトル --></header>
  <main>
    <canvas id="app" width="800" height="600"></canvas>
    <aside class="controls">
      <!-- Left/Right/Up/Down: 選択移動 | Enter: カードプレイ | E: ターン終了 -->
    </aside>
  </main>
  <script type="module">
    import { initWebGPU, setupGlobalState, loadGameScript, showStartupError }
      from "./lib/web/kagura-init.js";
    import { installAudioHelpers } from "./lib/web/kagura-audio.js";
    import { installGfxHelpers } from "./lib/web/kagura-gfx.js";
    async function init() {
      const result = await initWebGPU("#app");
      if (!result) {
        showStartupError("#app", "WebGPU が必要です",
          "Chrome 113+ また��� Edge 113+ を使用してください。");
        return;
      }
      setupGlobalState(result.canvas, result.device, result.format, result.context);
      installAudioHelpers();
      installGfxHelpers();
      await loadGameScript("./_build/js/debug/build/main/main.js");
    }
    init().catch(e => showStartupError("#app", "起動エラー", String(e)));
  </script>
</body>
</html>
```

**Step 4: テスト作成**
- `test/browser/check_files.sh` — moon.mod.json, moonbit/, index.html, lib/web/*.js の存在確認
- `test/browser/check_html.sh` — `id="app"`, `width="800"`, kagura-init.js, main.js 参照を grep
- `test/browser/playwright_smoke.sh` — サーバー起動後に playwright-cli で開いてスクリーンショット

**Step 5: 実行確認**
```bash
bash test/browser/check_files.sh
bash test/browser/check_html.sh
python3 -m http.server 8080 &
playwright-cli open http://localhost:8080/
playwright-cli screenshot
playwright-cli close
```

---

### 到達経路・起動条件

| 項目 | 内容 |
|------|------|
| 利用者が到達する入口 | `http://localhost:8080/` （`python3 -m http.server 8080` 起動後） |
| 更新が必要な呼び出し元・配線 | なし（既存 Haskell コードへの影響ゼロ） |
| 起動条件 | WebGPU 対応ブラウザ（Chrome 113+ / Edge 113+）が必要 |
| 未対応項目 | なし |

---

## 実装ガイドライン

- **`id="app"` は変更禁止** — `moonbit/main/main.mbt:9` で `canvas="#app"` が固定
- **`<script type="module">` 必須** — `import` 文を含むため、type なし script では構文エラー
- **初期化順序を守る** — `setupGlobalState` → `installAudioHelpers` → `installGfxHelpers` → `loadGameScript`（`kagura-init.js:87-114` 参照。順序違反で `globalThis.__kaguraWebRuntime` 未設定クラッシュ）
- **静的サーバー経由が必須** — `file://` では ES module の CORS 制限で `import` が失敗する
- **kagura lib/web の参照元** — `.mooncakes/mizchi/kagura/lib/web/`。バージョンを固定するため `.mooncakes/` からコピーし `lib/web/` に配置する

---

## スコープ外

| 項目 | 除外理由 |
|------|---------|
| Vite / vite-plugin-moonbit によるホットリロード | タスク要件にない。静的サーバーで十分 |
| moon install によるオンライン依存取得 | `.mooncakes/` がローカルに存在するため不要 |
| モバイルレイアウト | canvas 800×600 固定のためゲームロジック側変更なしでは根本的対応不可。説明テキストのレスポンシブのみ対応 |
| WASM ビルド | `preferred-target: js` の JS ビルドで要件を満たす |

---

## 確認事項

なし。コードと設定から全ての必要情報を確認済み。