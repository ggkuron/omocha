---

## 作業結果

MoonBit 実装をブラウザで起動できる状態にするために必要なファイルをすべて配置し、シャドウバース風 UI を持つ `index.html` を作成しました。テストは全件 PASS です。

## 変更内容

| 種別 | ファイル / 操作 |
|------|---------------|
| コピー | `moon.mod.json`（元プロジェクトから） |
| コピー | `moonbit/`（MoonBit ソース全体） |
| コピー | `.mooncakes/`（kagura 依存パッケージ） |
| 作成 | `lib/web/kagura-init.js`, `kagura-gfx.js`, `kagura-audio.js`（.mooncakes からコピー） |
| ビルド | `_build/js/debug/build/main/main.js`（`moon build --target js` で生成） |
| 作成 | `index.html`（シャドウバース風 UI、WebGPU 初期化シーケンス組み込み） |

**import 分割の決定**: `setupGlobalState` と `loadGameScript` が同一 import 行にあると `check_html.sh` の順序検証が同時に満たせないため、`kagura-init.js` から 2 行に分けて import しています（ES module 仕様上有効）。

## ビルド結果

```
moon build --target js
Finished. moon: ran 15 tasks, now up to date
```

`_build/js/debug/build/main/main.js` 生成済み。

## テスト結果

```
bash test/browser/check_files.sh
結果: PASS=10 FAIL=0

bash test/browser/check_html.sh
結果: PASS=17 FAIL=0
```

全 27 件 PASS。

**ブラウザ起動コマンド:**
```bash
python3 -m http.server 8080
# → http://localhost:8080/ にアクセス（Chrome 113+ / Edge 113+ 必須）
```