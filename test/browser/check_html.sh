#!/usr/bin/env bash
# index.html の内容検証テスト
# 以下を確認する:
#   - <canvas id="app" width="800" height="600"> が存在する
#   - kagura-init.js への参照が存在する
#   - main.js ビルド成果物への参照が存在する
#   - <script type="module"> が使われている
#   - WebGPU 初期化の正しい順序 (setupGlobalState → installAudioHelpers → installGfxHelpers → loadGameScript) が存在する

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HTML="${REPO_ROOT}/index.html"

PASS=0
FAIL=0

check_contains() {
  local label="$1"
  local pattern="$2"
  if grep -qE "${pattern}" "${HTML}" 2>/dev/null; then
    echo "  PASS: ${label}"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: ${label} (pattern: ${pattern})"
    FAIL=$((FAIL + 1))
  fi
}

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

echo "=== check_html: index.html 内容検証 ==="

if [ ! -f "${HTML}" ]; then
  echo "  FAIL: index.html が存在しない"
  echo ""
  echo "結果: PASS=0 FAIL=1"
  exit 1
fi

# canvas 要素
check_contains 'canvas id="app" が存在する'        'id="app"'
check_contains 'canvas width="800" が存在する'     'width="800"'
check_contains 'canvas height="600" が存在する'    'height="600"'
check_contains '<canvas> 要素が存在する'           '<canvas'

# ES module script
check_contains 'type="module" が使われている'      'type="module"'

# kagura JS の参照
check_contains 'kagura-init.js への参照が存在する' 'kagura-init\.js'
check_contains 'kagura-gfx.js への参照が存在する'  'kagura-gfx\.js'
check_contains 'kagura-audio.js への参照が存在する' 'kagura-audio\.js'

# MoonBit ビルド成果物への参照
check_contains 'main.js への参照が存在する'         'main\.js'

# WebGPU 初期化関数の呼び出し
check_contains 'setupGlobalState が呼ばれている'   'setupGlobalState'
check_contains 'installAudioHelpers が呼ばれている' 'installAudioHelpers'
check_contains 'installGfxHelpers が呼ばれている'   'installGfxHelpers'
check_contains 'loadGameScript が呼ばれている'      'loadGameScript'

# 初期化順序: setupGlobalState → installAudioHelpers（呼び出し行のみ対象）
check_order 'setupGlobalState は installAudioHelpers より前に呼ばれる' \
  'setupGlobalState\(' 'installAudioHelpers\('

# 初期化順序: installGfxHelpers → loadGameScript（呼び出し行のみ対象）
check_order 'installGfxHelpers は loadGameScript より前に呼ばれる' \
  'installGfxHelpers\(' 'loadGameScript\('

# シャドウバース風 UI 要素
check_contains 'ダークテーマ CSS が存在する (background または background-color)' \
  'background'
check_contains '操作説明の参照が存在する (controls または キーボード説明)' \
  'Left|Right|Enter|controls|矢印|キー'

echo ""
echo "結果: PASS=${PASS} FAIL=${FAIL}"

if [ "${FAIL}" -gt 0 ]; then
  exit 1
fi
