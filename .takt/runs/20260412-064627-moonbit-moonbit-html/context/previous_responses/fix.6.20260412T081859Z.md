全テストが通過しました。

---

## 作業結果

- `lib/web/kagura-init.js:90` のデッドフィールド `selector: "#app"` を削除した
- `test/browser/check_files.sh` に `selector` フィールドの再発防止テスト（`check_no_field`）を追加した

## 変更内容

- **`lib/web/kagura-init.js`**: `setupGlobalState` 内の状態オブジェクト初期化から `selector: "#app",` 行を削除
- **`test/browser/check_files.sh`**: `check_no_field` 関数を追加し、`kagura-init.js` に `selector:` フィールドが存在しないことを検証するテストを追加（PASS=17→18）

## ビルド結果

- MoonBit ビルド成果物 `_build/js/debug/build/main/main.js` 存在確認済み（check_files.sh PASS=18 FAIL=0）

## テスト結果

```
bash test/browser/check_files.sh  → PASS=18 FAIL=0
bash test/browser/check_html.sh   → PASS=17 FAIL=0
moon test                         → Total tests: 79, passed: 79, failed: 0
```

## 収束ゲート

| 指標 | 件数 |
|------|------|
| new（今回修正） | 1 |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0 |

## 証拠

- `lib/web/kagura-init.js:88-105` を直接読み取り、`selector` フィールドが削除されていることを確認
- `check_files.sh` 実行: `PASS: kagura-init.js に selector デッドフィールドが存在しない` → PASS=18 FAIL=0
- `check_html.sh` 実行: PASS=17 FAIL=0
- `moon test` 実行: Total 79, passed 79, failed 0