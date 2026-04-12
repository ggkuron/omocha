---

## 作業結果
- `lib/web/kagura-gfx.js` の内部実装関数4件から不要な `export` キーワードを削除した
- 対象: `ensureGpuPipeline`（L568）・`releaseGpuResources`（L698）・`finalizeTextureUpload`（L786）・`renderGpu`（L1318）
- これらはいずれも `installGfxHelpers` 内でファイル内参照のみ使用されており、外部 import は存在しない

## 変更内容
- `lib/web/kagura-gfx.js`: 4関数の `export function` → `function` に変更
- `installGfxHelpers`（L1594）のパブリック API は変更なし

## ビルド結果
- `moon build --target js` → `Finished. moon: no work to do`（MoonBit は変更なし、正常）

## テスト結果
- `check_files.sh` → **PASS=10 FAIL=0**
- `check_html.sh` → **PASS=17 FAIL=0**

## 収束ゲート
| 指標 | 件数 |
|------|------|
| new（今回修正） | 1 |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0 |

## 証拠
- `grep '^export function' lib/web/kagura-gfx.js` → `installGfxHelpers` のみ残存（内部4関数のエクスポート削除を確認）
- `check_files.sh` PASS=10 FAIL=0、`check_html.sh` PASS=17 FAIL=0 を実行確認
- `moon build --target js` 正常終了