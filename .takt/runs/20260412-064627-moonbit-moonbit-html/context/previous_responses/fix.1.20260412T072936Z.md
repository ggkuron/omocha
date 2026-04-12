---

## 作業結果
- `lib/web/kagura-gfx.js` の `scheduleTimestampReadback` 関数内にあった空 catch を3箇所すべて警告ログ付きに修正
- `test/browser/check_html.sh` の `check_order` 関数を awk ベースの壊れた実装から `grep -nE` によるファイル全体検索に置き換え、さらに関数呼び出しパターンに `\(` を追加して import 文とのマッチを防いだ

## 変更内容
- **`lib/web/kagura-gfx.js`** (旧 L283, L293-296)
  - L283: `catch (_) {}` → `catch (unmapError) { console.warn("[kagura] GPU buffer unmap error (then):", unmapError); }`
  - L293: `.catch(() => {` → `.catch((e) => { console.warn("[kagura] GPU timestamp readback error:", e);`
  - L296: `catch (_) {}` → `catch (unmapError) { console.warn("[kagura] GPU buffer unmap error (catch):", unmapError); }`
- **`test/browser/check_html.sh`**
  - `check_order` 関数: awk 範囲パターン（途中で終端していた）を削除し、`grep -nE "${first}" "${HTML}"` によるファイル全体検索に変更
  - `check_order` 呼び出し側: パターンを `setupGlobalState` → `setupGlobalState\(` 等、関数呼び出し形式に変更（import 行を誤マッチしないよう）

## ビルド結果
- JavaScript ファイルのため型チェックコンパイルなし（moon build は前ステップで Finished 済み）

## テスト結果
- `bash test/browser/check_html.sh` → **PASS=17 FAIL=0**
- `bash test/browser/check_files.sh` → **PASS=10 FAIL=0**

## 収束ゲート
| 指標 | 件数 |
|------|------|
| new（今回修正） | 4（AI-NEW×3 + ARCH-NEW×1） |
| reopened（再発修正） | 0 |
| persists（前回から継続、今回未着手） | 0 |

## 証拠
- `lib/web/kagura-gfx.js:283` `catch (unmapError)` + `console.warn(...)` 確認済み
- `lib/web/kagura-gfx.js:295` `.catch((e) => {` + `console.warn("[kagura] GPU timestamp readback error:", e)` 確認済み
- `lib/web/kagura-gfx.js:299` `catch (unmapError)` + `console.warn("[kagura] GPU buffer unmap error (catch):", unmapError)` 確認済み
- `test/browser/check_html.sh:30-52` awk 除去・grep 置換・`\(` パターン確認済み
- 両テストスクリプトとも全 PASS 確認済み