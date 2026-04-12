#!/usr/bin/env bash
# ブラウザ起動に必要なファイルの存在確認テスト
# 実装前は失敗する（想定内）。実装後に全件パスすること。

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

PASS=0
FAIL=0

check() {
  local label="$1"
  local path="$2"
  if [ -e "${REPO_ROOT}/${path}" ]; then
    echo "  PASS: ${label} (${path})"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: ${label} (${path}) - not found"
    FAIL=$((FAIL + 1))
  fi
}

# dead-field 再発防止: 参照されないフィールドが状態オブジェクトに残存していないことを確認する
check_no_field() {
  local label="$1"
  local file="$2"
  local pattern="$3"
  if grep -qE "${pattern}" "${REPO_ROOT}/${file}" 2>/dev/null; then
    echo "  FAIL: ${label} (${file} にデッドフィールドが存在する)"
    FAIL=$((FAIL + 1))
  else
    echo "  PASS: ${label}"
    PASS=$((PASS + 1))
  fi
}

# unnecessary-export 再発防止: 内部実装関数が export されていないことを確認する
check_no_export() {
  local label="$1"
  local file="$2"
  local func="$3"
  if grep -qE "^export function ${func}" "${REPO_ROOT}/${file}" 2>/dev/null; then
    echo "  FAIL: ${label} (${file} に 'export function ${func}' が存在する)"
    FAIL=$((FAIL + 1))
  else
    echo "  PASS: ${label}"
    PASS=$((PASS + 1))
  fi
}

echo "=== check_files: ブラウザ起動ファイル存在確認 ==="

# MoonBit プロジェクト設定
check "moon.mod.json が存在する"                  "moon.mod.json"

# MoonBit ソース
check "moonbit/ ディレクトリが存在する"            "moonbit"
check "moonbit/main/ ディレクトリが存在する"       "moonbit/main"
check "moonbit/main/main.mbt が存在する"           "moonbit/main/main.mbt"

# ビルド成果物
check "main.js ビルド成果物が存在する"             "_build/js/debug/build/main/main.js"

# kagura エンジン JS (lib/web/ コピー済み)
check "lib/web/ ディレクトリが存在する"            "lib/web"
check "kagura-init.js が存在する"                  "lib/web/kagura-init.js"
check "kagura-gfx.js が存在する"                   "lib/web/kagura-gfx.js"
check "kagura-audio.js が存在する"                 "lib/web/kagura-audio.js"

# ブラウザ起動エントリポイント
check "index.html が存在する"                      "index.html"

# unnecessary-export 再発防止: 内部実装関数が export されていないことを確認する
# kagura-audio.js: installAudioHelpers のみが外部 API。内部実装関数は export 禁止
check_no_export "createAudioBackend は export されていない" \
  "lib/web/kagura-audio.js" "createAudioBackend"
check_no_export "advanceAudioWrite は export されていない" \
  "lib/web/kagura-audio.js" "advanceAudioWrite"
check_no_export "closeAudio は export されていない" \
  "lib/web/kagura-audio.js" "closeAudio"
# kagura-gfx.js: installGfxHelpers のみが外部 API。内部実装関数は export 禁止
check_no_export "ensureGpuPipeline は export されていない" \
  "lib/web/kagura-gfx.js" "ensureGpuPipeline"
check_no_export "releaseGpuResources は export されていない" \
  "lib/web/kagura-gfx.js" "releaseGpuResources"
check_no_export "finalizeTextureUpload は export されていない" \
  "lib/web/kagura-gfx.js" "finalizeTextureUpload"
check_no_export "renderGpu は export されていない" \
  "lib/web/kagura-gfx.js" "renderGpu"

# dead-field 再発防止: kagura-init.js の state に参照されないフィールドが存在しないことを確認する
check_no_field "kagura-init.js に selector デッドフィールドが存在しない" \
  "lib/web/kagura-init.js" "selector:"

echo ""
echo "結果: PASS=${PASS} FAIL=${FAIL}"

if [ "${FAIL}" -gt 0 ]; then
  exit 1
fi
