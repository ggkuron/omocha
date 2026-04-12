## arch-review
これで分析に必要なすべての情報が揃いました。レポートを作成します。

---

## アーキテクチャレビュー結果

### 前回 open findings の追跡

前回の「Previous Response」は `ai-review.2` からのものであり、arch-review として前回の実行はありません（Step Iteration: 1）。AI レビューの open findings を参照用に記録します。

| finding_id | source | 今回の状態 | 根拠 |
|------------|--------|-----------|------|
| AI-NEW-kagura-gfx-js-L283 | ai-review | persists | `lib/web/kagura-gfx.js:283` `} catch (_) {}` 未変更 |
| AI-NEW-kagura-gfx-js-L293 | ai-review | persists | `lib/web/kagura-gfx.js:293` `.catch(() => {` 未変更 |
| AI-NEW-kagura-gfx-js-L296 | ai-review | persists | `lib/web/kagura-gfx.js:296` `} catch (_) {}` 未変更 |

これらは ai-review の担当領域のため、本レポートでは追跡記録のみとし再判定しない。

---

### 新規 arch-review 発見

---

#### ❌ REJECT — ARCH-NEW-1: `check_order` の awk パターンが `init()` 本体を正しく抽出できない

**ファイル**: `test/browser/check_html.sh:39`

**問題の詳細**:

```bash
init_body=$(awk '/async function init\(\)/,/^\s*\}/' "${HTML}" 2>/dev/null)
```

awk の範囲パターン `/start/,/end/` の終端条件 `^\s*\}` は「行頭から始まるオプション空白 + `}`」にマッチする。`index.html` の構造は以下の通り:

```
114:    async function init() {        ← 範囲開始
115:      const result = await initWebGPU("#app");
116:      if (!result) {
117-121:    showStartupError(...)
122:        return;
123:      }                             ← ^\s*\} にマッチ → 範囲終了 ← ここで終わる
124:      setupGlobalState(...)         ← 抽出されない
125:      installAudioHelpers();        ← 抽出されない
126:      installGfxHelpers();          ← 抽出されない
127:      await loadGameScript(...);    ← 抽出されない
128:    }
```

`init()` 内部の `if (!result)` ブロックを閉じる `}` (L123) が終端パターンにマッチして範囲が終了する。L124-L127 の呼び出しはすべて `init_body` に含まれない。

**影響**: `check_html.sh:87-92` の2つの `check_order` テストが、正しい実装に対しても必ず FAIL を報告する:

```
FAIL: setupGlobalState は installAudioHelpers より前に呼ばれる
      (init() 本体内で片方または両方が見つからない)
FAIL: installGfxHelpers は loadGameScript より前に呼ばれる
      (init() 本体内で片方または両方が見つからない)
```

テストが仕様を正しく検証できていない。これは「テストがない新しい振る舞い」と等価の問題（振る舞いを検証しているつもりで検証できていない）。

**修正案 — `test/browser/check_html.sh` の `check_order` 関数内 `init_body` 抽出部分を以下に変更**:

```bash
# 変更前 (L39)
init_body=$(awk '/async function init\(\)/,/^\s*\}/' "${HTML}" 2>/dev/null)

# 変更後: 全行から順序を確認（init() は1つしか存在しないため十分）
# init_body は不要。line_first / line_second を HTML 全体から取得する
```

`check_order` 関数全体を下記に置き換える:

```bash
check_order() {
  local label="$1"
  local first="$2"
  local second="$3"
  local line_first
  local line_second
  line_first=$(grep -nE "${first}" "${HTML}" | head -1 | cut -d: -f1 || echo "")
  line_second=$(grep -nE "${second}" "${HTML}" | head -1 | cut -d: -f1 || echo "")
  if [ -z "${line_first}" ] || [ -z "${line_second}" ]; then
    echo "  FAIL: ${label} (片方または両方が見つからない: first=${first}, second=${second})"
    FAIL=$((FAIL + 1))
  elif [ "${line_first}" -lt "${line_second}" ]; then
    echo "  PASS: ${label}"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: ${label} (順序が逆: ${first} は行${line_first}, ${second} は行${line_second})"
    FAIL=$((FAIL + 1))
  fi
}
```

`init()` は HTML に1つしか存在しないため、ファイル全体での行番号比較は init() 内の順序と等価であり、検証として十分。

---

### 非ブロッキング（参考情報）

#### ⚠️ WARNING — ARCH-NOTE-1: `lib/web/` は upstream vendor copy でありバージョンズレリスクがある

`lib/web/kagura-gfx.js` (1619行)、`kagura-audio.js`、`kagura-init.js` はすべて `.mooncakes/mizchi/kagura/lib/web/` の verbatim copy（各1箇所のみ修正）。

- `coder-decisions.md` に同期戦略の記録なし
- `.mooncakes` が更新された場合 `lib/web/` との乖離が生じる
- タスクスコープを大きく逸脱するため今回は非ブロッキング。次タスクで `lib/web/` のコピー元管理ポリシーを明記することを推奨。

---

### 判定

## 結果: **REJECT**

### ブロッキング問題

| finding_id | 場所 | 問題 | 修正難度 |
|------------|------|------|---------|
| ARCH-NEW-1 | `test/browser/check_html.sh:39` | `check_order` の awk パターンが init() 内の `if` ブロック closing brace で終端し、`setupGlobalState`/`installAudioHelpers`/`installGfxHelpers`/`loadGameScript` が抽出されず、両順序テストが正しい実装に対しても FAIL を報告する | 低（関数1本の差し替え） |

修正は `check_order` 関数の `init_body` 抽出を廃止して `grep -n` に置き換えるだけ（5行の変更）。上記の修正案を適用してください。

---

## supervise
必要な情報が揃いました。最終検証結果を出力します。

---

# 最終検証結果

## 結果: REJECT

---

## 要件充足チェック

| # | 要件（タスク指示書から抽出） | 充足 | 根拠（ファイル:行） |
|---|---------------------------|------|-------------------|
| 1 | `moonbit/` 配下の生成済み MoonBit 実装をブラウザで起動できる状態にする | ✅ | `moonbit/` コピー済み、`_build/js/debug/build/main/main.js` 生成済み — `check_files.sh` PASS=10（実行証跡: implement.1.20260412T070631Z.md） |
| 2 | ブラウザ起動用の HTML を用意する | ✅ | `index.html` 存在 — `check_files.sh` PASS（実行証跡: 本次 check_files.sh 実行 PASS=10） |
| 3 | HTML から MoonBit の生成物または既存の表示ロジックを正しく読み込む | ✅ | `index.html:109-112` で kagura JS 3本を import、`index.html:127` で `loadGameScript("./_build/js/debug/build/main/main.js")` 呼び出し — `check_html.sh` PASS=17 |
| 4 | ローカルで画面を開ける起動コマンドとアクセス URL を明確にする | ✅ | `python3 -m http.server 8080` / `http://localhost:8080/` を implement.1.md に明記済み |
| 5 | `playwright-cli` を使って実画面を確認しながら UI を調整する | ❌ | implement.1.md にスクリーンショット確認の証跡なし。`playwright_smoke.sh` は「未インストールの場合 SKIP (exit 0)」設計のため、playwright が実際に実行された証跡がこの run に存在しない |
| 6 | UI はシャドウバース風のカードバトル UI に寄せる | ✅ | `index.html:11` ダークグラデーション背景、`index.html:54-60` 金色グローボーダー、`index.html:64-91` 操作説明パネル実装済み |
| 7 | 実装後、ビルド・テスト・ブラウザ表示確認を実行する（3条件に分解） | | |
| 7a | ビルドを実行する | ✅ | `moon build --target js` "Finished. ran 15 tasks" — implement.1.md に証跡あり |
| 7b | テストを実行する | ✅ | `check_files.sh` PASS=10 / `check_html.sh` PASS=17 — 今次実行で確認済み |
| 7c | ブラウザ表示確認を実行する | ❌ | playwright による実画面確認の証跡がこの run に存在しない（未確認） |

**❌ が2件（要件5, 7c）あるため REJECT。**

---

## 前段 finding の再評価

| finding_id | 前段判定 | 再評価 | 根拠 |
|------------|----------|--------|------|
| AI-NEW-kagura-gfx-js-L283 | new (REJECT) | **妥当** | `lib/web/kagura-gfx.js:283` に `} catch (_) {}` が実在確認済み。当該ファイルは L401 を修正しており変更対象ファイル。ポリシー「エラーの握りつぶし（空の catch）」に該当 |
| AI-NEW-kagura-gfx-js-L293 | new (REJECT) | **妥当** | `lib/web/kagura-gfx.js:293` に `.catch(() => {` が実在確認済み（エラーパラメータなし）。readback 失敗が無音で消える |
| AI-NEW-kagura-gfx-js-L296 | new (REJECT) | **妥当** | `lib/web/kagura-gfx.js:296` に `} catch (_) {}` が実在確認済み。同ブロック内の unmap() 例外を完全に握りつぶし |
| AI-NEW-index-html-L109-L113 | resolved | **妥当** | `index.html:109-110` で統合済み。`check_html.sh:39` の `check_order` が `awk` で init() 本体を抽出するよう修正済みを確認 |
| AI-NEW-kagura-audio-js-L82 | resolved | **妥当** | `lib/web/kagura-audio.js:82` で `.catch((e) => { console.warn(...) })` に修正済み（ai-fix.1 証跡） |
| AI-NEW-kagura-gfx-js-L401 | resolved | **妥当** | `lib/web/kagura-gfx.js:401` で `.catch((e) => { console.warn(...) })` に修正済みを直接確認 |

---

## 検証サマリー

| 項目 | 状態 | 確認方法 |
|------|------|---------|
| テスト | ✅ | `check_files.sh` PASS=10 / `check_html.sh` PASS=17（本次実行） |
| ビルド | ✅ | `moon build --target js` "ran 15 tasks" — implement.1.20260412T070631Z.md |
| ブラウザ表示確認（playwright） | ❌ | 実行証跡なし（未確認）。`playwright_smoke.sh` は SKIP 設計のため実際の playwright 実行が行われたか判断不能 |

---

## 成果物

- 作成: `index.html`, `lib/web/kagura-init.js`, `lib/web/kagura-gfx.js`, `lib/web/kagura-audio.js`, `moon.mod.json`, `moonbit/`, `.mooncakes/`, `_build/js/debug/build/main/main.js`, `test/browser/check_files.sh`, `test/browser/check_html.sh`, `test/browser/playwright_smoke.sh`
- 変更（ai-fix）: `index.html`, `test/browser/check_html.sh`, `lib/web/kagura-audio.js`, `lib/web/kagura-gfx.js`

---

## 未完了項目（REJECT）

| # | 項目 | 理由 |
|---|------|------|
| 1 | `lib/web/kagura-gfx.js:283` — `catch (_) {}` を `catch (unmapError) { console.warn("[kagura] GPU buffer unmap error (then):", unmapError); }` に修正 | ボーイスカウトルール：変更対象ファイル内の空 catch（エラー握りつぶし、ポリシー REJECT 基準） |
| 2 | `lib/web/kagura-gfx.js:293` — `.catch(() => {` を `.catch((e) => { console.warn("[kagura] GPU timestamp readback error:", e);` に修正（クリーンアップ続行） | 同上：エラーパラメータなしで readback 失敗が無音消失 |
| 3 | `lib/web/kagura-gfx.js:296` — `catch (_) {}` を `catch (unmapError) { console.warn("[kagura] GPU buffer unmap error (catch):", unmapError); }` に修正 | 同上：空 catch |
| 4 | `playwright-cli` による実画面確認・スクリーンショット取得（要件5 / 要件7c） | タスク指示書に明示された必須確認ステップの実行証跡がこの run に存在しない。`playwright_smoke.sh` を実行し実際に playwright が動作することを確認すること（または playwright 未インストールを明示したうえで代替確認を行うこと） |