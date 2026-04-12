#!/usr/bin/env bash
# Playwright を使ったブラウザ表示確認スクリプト
# 前提: index.html を静的サーバーで配信中であること
#   python3 -m http.server 8080 &
# 使用方法:
#   bash test/browser/playwright_smoke.sh [PORT]
#   PORT のデフォルト値は 8080

set -euo pipefail

PORT="${1:-8080}"
BASE_URL="http://localhost:${PORT}"
SCREENSHOT_DIR="test/browser/screenshots"
TIMESTAMP="$(date +%Y%m%dT%H%M%S)"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

echo "=== playwright_smoke: ブラウザ表示確認 ==="
echo "  URL: ${BASE_URL}"

# playwright-cli の存在確認
if ! command -v playwright-cli &>/dev/null && ! command -v playwright &>/dev/null; then
  echo "  SKIP: playwright-cli または playwright が見つかりません"
  echo "  インストール: npm install -g playwright-cli"
  exit 0
fi

PLAYWRIGHT_CMD=""
if command -v playwright-cli &>/dev/null; then
  PLAYWRIGHT_CMD="playwright-cli"
else
  PLAYWRIGHT_CMD="playwright"
fi

# スクリーンショット保存先
mkdir -p "${REPO_ROOT}/${SCREENSHOT_DIR}"
SCREENSHOT_PATH="${REPO_ROOT}/${SCREENSHOT_DIR}/smoke-${TIMESTAMP}.png"
MOBILE_SCREENSHOT_PATH="${REPO_ROOT}/${SCREENSHOT_DIR}/smoke-mobile-${TIMESTAMP}.png"

echo "  playwright コマンド: ${PLAYWRIGHT_CMD}"
echo "  スクリーンショット保存先: ${SCREENSHOT_PATH}"

# playwright-cli (Anthropic版) と標準 playwright で分岐
if "${PLAYWRIGHT_CMD}" --version 2>&1 | grep -q "^0\."; then
  # -- playwright-cli (Anthropic版) の場合 --
  # Given: ローカル静的サーバーが http://localhost:PORT で起動している
  # When: playwright-cli でブラウザを開いてスクリーンショットを取得する
  # Then: スクリーンショットファイルが生成される

  "${PLAYWRIGHT_CMD}" open "${BASE_URL}/"
  trap '"${PLAYWRIGHT_CMD}" close 2>/dev/null || true' EXIT
  "${PLAYWRIGHT_CMD}" resize 1280 800
  "${PLAYWRIGHT_CMD}" screenshot --filename "${SCREENSHOT_PATH}"

  if [ -f "${SCREENSHOT_PATH}" ]; then
    echo "  PASS: デスクトップスクリーンショット取得成功 -> ${SCREENSHOT_PATH}"
  else
    echo "  FAIL: スクリーンショットが生成されなかった"
    exit 1
  fi

  "${PLAYWRIGHT_CMD}" resize 375 812
  "${PLAYWRIGHT_CMD}" screenshot --filename "${MOBILE_SCREENSHOT_PATH}"

  if [ -f "${MOBILE_SCREENSHOT_PATH}" ]; then
    echo "  PASS: モバイルスクリーンショット取得成功 -> ${MOBILE_SCREENSHOT_PATH}"
  else
    echo "  FAIL: モバイルスクリーンショットが生成されなかった"
    exit 1
  fi
else
  # -- 標準 playwright CLI の場合 --
  # Given: ローカル静的サーバーが http://localhost:PORT で起動している
  # When: playwright screenshot でスクリーンショットを取得する
  # Then: スクリーンショットファイルが生成される

  "${PLAYWRIGHT_CMD}" screenshot \
    --viewport-size "1280,800" \
    "${BASE_URL}/" \
    "${SCREENSHOT_PATH}"

  if [ -f "${SCREENSHOT_PATH}" ]; then
    echo "  PASS: デスクトップスクリーンショット取得成功 -> ${SCREENSHOT_PATH}"
  else
    echo "  FAIL: スクリーンショットが生成されなかった"
    exit 1
  fi

  "${PLAYWRIGHT_CMD}" screenshot \
    --viewport-size "375,812" \
    "${BASE_URL}/" \
    "${MOBILE_SCREENSHOT_PATH}"

  if [ -f "${MOBILE_SCREENSHOT_PATH}" ]; then
    echo "  PASS: モバイルスクリーンショット取得成功 -> ${MOBILE_SCREENSHOT_PATH}"
  else
    echo "  FAIL: モバイルスクリーンショットが生成されなかった"
    exit 1
  fi
fi

echo ""
echo "結果: PASS スクリーンショット確認完了"
echo "  -> ${SCREENSHOT_PATH}"
echo "  -> ${MOBILE_SCREENSHOT_PATH}"
