---

## 作業結果
- `lib/web/kagura-audio.js`の内部実装関数3件（`createAudioBackend`・`advanceAudioWrite`・`closeAudio`）から不要な`export`を削除
- `test/browser/check_files.sh`に`unnecessary-export`ファミリーの再発防止テスト7件を追加（audio/gfx 両ファイルをカバー）

## 変更内容
- `lib/web/kagura-audio.js` L42・L91・L108: `export function` → `function`（`installAudioHelpers`のL118のexportは変更なし）
- `test/browser/check_files.sh`: `check_no_export`ヘルパー関数を追加、kagura-audio.js 3関数・kagura-gfx.js 4関数の不要exportチェックを追加

## ビルド結果
- `moon test`: Total tests: 79, passed: 79, failed: 0 ✅

## テスト結果
- `bash test/browser/check_files.sh`: PASS=17 FAIL=0 ✅
- `bash test/browser/check_html.sh`: PASS=17 FAIL=0 ✅
- `moon test`: 79/79 passed ✅

## 収束ゲート
| 指標 | 件数 |
|------|------|
| new（今回修正） | 4（ARCH: 1件 + AI: 3件 ※gfx.jsは既修正を確認） |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0 |

## 証拠
- `grep '^export function' lib/web/kagura-audio.js` → `installAudioHelpers`（L118）のみ ✅
- `lib/web/kagura-gfx.js` L283/295/296のempty-catchはai_fixステップで修正済みを確認（`catch (unmapError)` + `console.warn` が存在）
- `check_files.sh`のPASS=17 / FAIL=0（再発防止テスト7件含む）