`loadFonts` 関数と `state.fonts` の初期化が削除されました。

---

## 作業結果

- **ARCH-NEW-kagura-init-js-L116（new）**: `lib/web/kagura-init.js` の dead export `loadFonts` 関数（L116-131）と関連する `state.fonts = state.fonts || {}` 初期化（L106）を削除
- **VAL-PERSIST-playwright-confirm（persists）**: `playwright_smoke.sh` を `playwright-cli`（Anthropic版、v0.1.7）の正しい API に修正（`open`→`resize`→`screenshot`→`close`）し、静的サーバー起動後に実行。PASS 確認

## 変更内容

- `lib/web/kagura-init.js`: `state.fonts = state.fonts || {};`（L106）と `export async function loadFonts(entries) { ... }`（L116-131）を削除
- `test/browser/playwright_smoke.sh`: `playwright-cli` のバージョン検出を追加し、Anthropic版は `open`→`resize`→`screenshot --filename`→`close` シーケンスに修正（`--viewport-size` オプションを使わない形式）

## ビルド結果

- `check_files.sh` PASS=10 FAIL=0
- `check_html.sh` PASS=17 FAIL=0（`loadFonts` 削除後も HTML テストは全通過）

## テスト結果

```
bash test/browser/check_files.sh  → PASS=10 FAIL=0
bash test/browser/check_html.sh   → PASS=17 FAIL=0
bash test/browser/playwright_smoke.sh
  → デスクトップ PASS: smoke-20260412T163829.png 取得成功
  → モバイル PASS: smoke-mobile-20260412T163829.png 取得成功
  → 結果: PASS
```

ブラウザ表示確認スクリーンショット: シャドウバース風ダークテーマ UI（ASTRATA CARD BATTLE）が正常表示。

## 収束ゲート

| 指標 | 件数 |
|------|------|
| new（今回修正） | 1（ARCH-NEW-kagura-init-js-L116） |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0（VAL-PERSIST-playwright-confirm を今回解消） |

## 証拠

- `lib/web/kagura-init.js:106` の `state.fonts = state.fonts || {};` 削除済み（確認済み）
- `lib/web/kagura-init.js:115-131` の `loadFonts` 関数全体削除済み（確認済み）
- `playwright_smoke.sh` 実行ログ: Page Title「Astrata – Card Battle」取得、スクリーンショット2件生成
- スクリーンショット画像: ダークグラジェント背景・金色タイトル・操作説明パネルが正常表示