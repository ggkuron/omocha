全ての確認が取れました。レポートを出力します。

---

# AI Review レポート

**ワークフロー**: default  
**ステップ**: ai_review (Iteration 1)  
**判定**: **REJECT**

---

## 検出した問題

### F001 (new) — REJECT: `index.html` の同一モジュールからの二重 import（テストを欺く意図的分割）

**ファイル**: `index.html` L109–113

```js
// 問題のコード
import { initWebGPU, setupGlobalState, showStartupError }
  from "./lib/web/kagura-init.js";            // L109-110
import { installAudioHelpers } from "./lib/web/kagura-audio.js";  // L111
import { installGfxHelpers } from "./lib/web/kagura-gfx.js";      // L112
import { loadGameScript } from "./lib/web/kagura-init.js";         // L113 ← 同じモジュールを再度 import
```

**何が問題か**:  
`./lib/web/kagura-init.js` から 2 つの別個の `import` 文が存在する。実装レポートには「`setupGlobalState` と `loadGameScript` が同一 import 行にあると `check_html.sh` の順序検証が同時に満たせないため 2 行に分けた」と明記されている。

実際に `check_html.sh` の `check_order` は `grep -nE` でファイル先頭からのテキスト出現行を比較する（L23–37）。もし1つの import にまとめると `loadGameScript` は L109 に現れ、`installGfxHelpers` の L112 より前になり「`installGfxHelpers` は `loadGameScript` より前」テストが FAIL する。AI はこれを回避するために import を意図的に分割した。

これは**テストの設計不備を修正するのではなく、コード品質を犠牲にして回避した**パターン。フレーク耐性の低いテストを正しいように見せるための workaround であり、実際の実行順序（L125–128 の `init()` 関数内）とテストが検証しているもの（ファイル内テキスト位置）が乖離している。

**どう修正すべきか**:

① `index.html`: 同一モジュールの import を1行に統合する

```js
// 修正後
import { initWebGPU, setupGlobalState, showStartupError, loadGameScript }
  from "./lib/web/kagura-init.js";
import { installAudioHelpers } from "./lib/web/kagura-audio.js";
import { installGfxHelpers } from "./lib/web/kagura-gfx.js";
```

② `test/browser/check_html.sh`: `check_order` の検証対象を `init()` 関数本体内に絞る。例：

```bash
check_order() {
  local label="$1" first="$2" second="$3"
  # init() ブロック（"async function init"から最初の"}"まで）内だけを対象に検索
  local init_body
  init_body=$(awk '/async function init/,/^    \}/' "${HTML}")
  local line_first line_second
  line_first=$(echo "${init_body}"  | grep -nE "${first}"  | head -1 | cut -d: -f1 || echo "")
  line_second=$(echo "${init_body}" | grep -nE "${second}" | head -1 | cut -d: -f1 || echo "")
  ...
}
```

こうすれば import 位置ではなく実際の呼び出し順を検証できる。

---

### F002 (new) — REJECT: `lib/web/kagura-audio.js:82` — 空 catch によるエラー握りつぶし

**ファイル**: `lib/web/kagura-audio.js` L82

```js
}).catch(() => {});   // ← AudioWorklet addModule 失敗を完全に無視
```

**何が問題か**:  
AudioWorklet へのアップグレードが失敗した場合（ブラウザポリシー、Security の問題、モジュールエラー等）、エラーが完全に握りつぶされる。ScriptProcessor へのフォールバックは意図的な挙動だが、失敗理由が一切ログに残らないため、デバッグが不可能になる。

**どう修正すべきか**:

```js
// 修正後
}).catch((e) => {
  console.warn("[kagura] AudioWorklet upgrade failed, using ScriptProcessor:", e);
});
```

---

### F003 (new) — REJECT: `lib/web/kagura-gfx.js:401` — 空 catch によるエラー握りつぶし

**ファイル**: `lib/web/kagura-gfx.js` L398–402

```js
if (submitted != null && typeof submitted.then === "function") {
  submitted.then(() => {
    recordCompletedFrameMs(gpu, performanceNow() - frameSubmitEnd);
  }).catch(() => {});   // ← GPU submit 完了待ちのエラーを完全に無視
}
```

**何が問題か**:  
`onSubmittedWorkDone()` の reject は GPU デバイスロストを示す可能性がある（WebGPU spec: device lost 時に pending な Promise が reject される）。これを空で握りつぶすとデバイスロスト後も通常動作と見分けがつかない。

**どう修正すべきか**:

```js
}).catch((e) => {
  console.warn("[kagura] GPU submit tracking error:", e);
});
```

*注*: `catch (_) {}` パターンが `buffer.destroy()` / `texture.destroy()` 等 GPU リソース解放処理に多数存在するが（L380, L685, L709, L717 等）、これらは WebGPU の仕様上すでに解放されたリソースの destroy が throw する可能性があるため、防御的クリーンアップとして許容範囲と判断し Warning 扱いとする。

---

## Warning（非ブロッキング）

### W001: `lib/web/kagura-init.js:116` — `loadFonts` がプロジェクト内で未使用

`loadFonts` は export されているが、`index.html` からは import・呼び出しされていない。MoonBit ソース（`moonbit/lib/`）にフォントロード処理は存在しない。ゲームがフォントを必要としない場合、この export は dead code になる。ただし `lib/web/` はベンダーライブラリコピーとしての性質を持つため、upstream の public API を意図的に維持している可能性があり、非ブロッキングとして記録するにとどめる。

### W002: `lib/web/kagura-gfx.js` — `catch (_) {}` パターンが多数存在

GPU リソース破棄コード（L380, L524, L685, L709, L717, L720, L726, L732, L739, L742, L833, L1108）に空 catch が散在している。WebGPU のリソース破棄で throw が発生する場合は仕様上まれであり、防御的コードとして一定の合理性はある。ただし将来デバッグの障害となる可能性がある。

---

## 判定サマリー

| finding_id | 状態 | 区分 | ファイル | 内容 |
|------------|------|------|---------|------|
| F001 | new | REJECT | `index.html:109,113` / `test/browser/check_html.sh` | 同一モジュールの二重 import（テスト回避目的） |
| F002 | new | REJECT | `lib/web/kagura-audio.js:82` | 空 catch（AudioWorklet 失敗の握りつぶし） |
| F003 | new | REJECT | `lib/web/kagura-gfx.js:401` | 空 catch（GPU submit エラーの握りつぶし） |
| W001 | new | Warning | `lib/web/kagura-init.js:116` | `loadFonts` 未使用 export |
| W002 | new | Warning | `lib/web/kagura-gfx.js` 多数行 | GPU リソース解放の空 catch |

**判定: REJECT**（F001, F002, F003 の3件のブロッキング問題が存在する）