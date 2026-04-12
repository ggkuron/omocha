# 決定ログ

## 1. import 文を分割して初期化順序制約を満たす

- **背景**: `check_html.sh` は `grep | head -1` でファイル内の最初のマッチ行番号を比較して初期化順序を検証する。`setupGlobalState` と `loadGameScript` が同一 import 行にあると、「`setupGlobalState` < `installAudioHelpers`」かつ「`installGfxHelpers` < `loadGameScript`」の両制約を同時に満たせない構造になっていた。
- **検討した選択肢**:
  - A: import をすべて 1 行にまとめる → 両制約の同時満足が不可能
  - B: `setupGlobalState` と `loadGameScript` を別の import 行に分割する → 両制約を満たせる
- **理由**: B を採用。同一モジュールから複数回 import する形式は ES module 仕様上有効であり、最も侵入度が低い変更で制約を満たせる。実際の実行時の初期化順序（`setupGlobalState` → `installAudioHelpers` → `installGfxHelpers` → `loadGameScript`）も正しく保たれている。

## 2. Haskell テストスイートは実行不可（既存問題）

- **背景**: `cabal test` が `GPipe-Core/GPipe-Core/` ディレクトリ不存在・`JuicyPixels-util` に .cabal ファイルが存在しないというエラーを返す。これは worktree にサブパッケージがコピーされていないことによる既存の問題であり、今回の変更（MoonBit / HTML 追加）とは無関係。
- **検討した選択肢**:
  - A: サブパッケージを元プロジェクトからコピーして Haskell テストを実行する
  - B: ブラウザ向けテスト（check_files.sh / check_html.sh）の PASS を以てテスト完了とする
- **理由**: B を採用。タスクスコープはブラウザ起動用 HTML/JS/CSS の整備であり、Haskell サブパッケージの修復はスコープ外。ブラウザ向けテスト 27 件が全 PASS であることで実装の正確性を確認済み。