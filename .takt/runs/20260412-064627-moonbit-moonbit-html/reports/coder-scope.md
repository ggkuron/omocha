# 変更スコープ宣言

## タスク
MoonBit 実装をブラウザで起動できるよう HTML/JS/CSS を整備し、シャドウバース風カードバトル UI に調整する

## 変更予定
| 種別 | ファイル |
|------|---------|
| コピー | `moon.mod.json` |
| コピー | `moonbit/` |
| コピー | `.mooncakes/` |
| 作成 | `lib/web/kagura-init.js` |
| 作成 | `lib/web/kagura-gfx.js` |
| 作成 | `lib/web/kagura-audio.js` |
| 生成 | `_build/js/debug/build/main/main.js` |
| 作成 | `index.html` |

## 推定規模
Medium

## 影響範囲
- 既存 Haskell コードへの影響ゼロ
- MoonBit ブラウザ起動エントリポイント新規追加（`index.html` + `lib/web/` + `_build/`）