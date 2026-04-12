# テスト作成レポート

## 作成テスト

| ファイル | 種別 | テスト数 | 概要 |
|---------|------|---------|------|
| `test/browser/check_files.sh` | 単体（シェル） | 10 | ブラウザ起動に必要なファイルの存在確認（moon.mod.json / moonbit/ / _build/ / lib/web/*.js / index.html） |
| `test/browser/check_html.sh` | 単体（シェル） | 17 | `index.html` の内容検証（canvas 要素・ES module・kagura JS 参照・WebGPU 初期化順序・UI 要素） |
| `test/browser/playwright_smoke.sh` | 統合（シェル） | 2 | Playwright によるデスクトップ・モバイルのスクリーンショット取得確認 |

## 実行結果（参考）

実装前のためテスト失敗・import エラーは想定内。

| 状態 | 件数 | 備考 |
|------|------|------|
| Pass | 0 | |
| Fail（想定内） | 11 | `check_files.sh` 10件・`check_html.sh` 1件。対象ファイル未作成のため |
| Error（要対応） | 0 | スクリプト自体の構文エラーなし |

## 備考

- **既存テスト基盤が Haskell / Hspec のみ**のため、ブラウザ向けテストはシェルスクリプトで実装した（Playwright テスト基盤なし）
- `playwright_smoke.sh` は `playwright-cli` / `playwright` が未インストールの場合 SKIP（exit 0）とし、CI 環境でエラーにならないよう配慮した
- `check_html.sh` は WebGPU 初期化の**順序**（`setupGlobalState` → `installAudioHelpers` → `installGfxHelpers` → `loadGameScript`）を行番号比較で検証する。順序違反は `__kaguraWebRuntime` 未設定クラッシュに直結するため
- `canvas id="app"` の検証を独立した項目として含めた。kagura エンジンが固定セレクタとして要求する契約であり、変更した場合はゲームが起動しない