テストが全件パスしました。

---

## 作業結果
- `ARCH-NEW-playwright-smoke-close-trap`（resource-leak）の指摘に対応。`test/browser/playwright_smoke.sh` の playwright-cli (Anthropic版) ブランチで、`open` 後に `trap` によるセッションクリーンアップを追加し、成功パスにのみ存在していた明示的 `close` を削除した。

## 変更内容
- `test/browser/playwright_smoke.sh` L51: `open` の直後に `trap '"${PLAYWRIGHT_CMD}" close 2>/dev/null || true' EXIT` を追加
- L71: 成功パスのみの `"${PLAYWRIGHT_CMD}" close` を削除（`trap` に一元化）

## ビルド結果
- MoonBit ビルドは前回実行済み（`_build/js/debug/build/main/main.js` 存在確認済み）。シェルスクリプトのみの変更のためビルド不要。

## テスト結果
- `bash test/browser/check_files.sh` → PASS=10 FAIL=0
- `bash test/browser/check_html.sh` → PASS=17 FAIL=0

## 収束ゲート
| 指標 | 件数 |
|------|------|
| new（今回修正） | 1 |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0 |

## 証拠
- `test/browser/playwright_smoke.sh:50-71` を直接確認し、`trap` 追加・明示的 `close` 削除を実施
- `check_files.sh` PASS=10 FAIL=0、`check_html.sh` PASS=17 FAIL=0（実行ログ上記）