全テストPASS。修正内容を報告します。

---

## 確認したファイル

- `index.html:109-113` — 同一モジュール `kagura-init.js` から2行に分割されたimport文が実在することを確認
- `test/browser/check_html.sh:30-49` — `check_order` がファイル全体を `grep` で検索する実装であることを確認
- `lib/web/kagura-audio.js:82` — `.catch(() => {})` の空catchが実在することを確認
- `lib/web/kagura-gfx.js:401` — `.catch(() => {})` の空catchが実在することを確認

## 実行した検索

- `grep` で各ファイルの対象行を確認（全問題が実在）

## 修正内容

1. **`index.html` L109-113**: `kagura-init.js` からの2つのimport文を1行に統合
   - `import { initWebGPU, setupGlobalState, showStartupError, loadGameScript } from "./lib/web/kagura-init.js";`
   - 分割import（テスト回避目的のコード歪め）を解消

2. **`test/browser/check_html.sh` L30-49**: `check_order` 関数を修正
   - `grep -nE` でファイル全体を検索 → `awk '/async function init\(\)/,/^\s*\}/'` で `init()` 本体を抽出してからその中で検索
   - 実際の実行順（init()本体内の呼び出し順）を正しく検証するように変更

3. **`lib/web/kagura-audio.js` L82**: `.catch(() => {})` → `.catch((e) => { console.warn("[kagura] AudioWorklet upgrade failed, using ScriptProcessor:", e); })`
   - AudioWorkletアップグレード失敗のエラーを握りつぶさないよう修正

4. **`lib/web/kagura-gfx.js` L401**: `.catch(() => {})` → `.catch((e) => { console.warn("[kagura] GPU submit tracking error:", e); })`
   - GPU submit追跡エラーを握りつぶさないよう修正

## テスト結果

```
bash test/browser/check_html.sh  → PASS=17 FAIL=0
bash test/browser/check_files.sh → PASS=10 FAIL=0
```